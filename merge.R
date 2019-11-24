suppressPackageStartupMessages({
  library(tidyverse)
  library(parallel)
})

# because speed is fun
numCores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(numCores, outfile = "")
parallel::clusterEvalQ(cl, {library(readxl)})

MergeFiles <- function(parentDirectory) {
  fileNames <- list.files(path = parentDirectory, full.names = TRUE)
  colNames <- c("Dev", "subid", "trial", "head", "pupil", "rightward", "rt")
  
  # Runs on each of the workers created in `cl`. 
  # For each file in list of files, apply the function that returns
  #   a cleaned df. Return object is list of cleaned dfs.
  mergeFilesList <- 
    parallel::parLapply(cl, fileNames, function(file) {
      participantNumber <- 
        as.numeric(gsub("[^0-9.-]", "", gsub("\\..*", "", basename(file))))
      taskType <- 
        substr(
          gsub("\\..*", "", basename(file)), 
          1, 
          1
        )
      
      print(sprintf("Merging: %s task from file: %s", taskType, file))
    
    df <- readxl::read_excel(file, range = cell_cols(1:7))
    print(nrow(unique(df$Dev)))
    if (participantNumber < 400) {
      asd <- 1
    } else {
      asd <- 0
    }
    if (taskType == "R") {
      robot <- 1
    } else {
      robot <- 0
    }
    
    names(df) <- colNames
    df$asd <- asd
    df$subid <- participantNumber
    df$robot <- robot
    return(df)
  })
  
  # rbind all dfs from list of clean dfs
  mergedFiles <- do.call(rbind.data.frame, mergeFilesList)
  return(mergedFiles)
}

computerFiles <- MergeFiles("computer_task")
robotFiles <- MergeFiles("robot_task")

df <- rbind.data.frame(robotFiles, computerFiles) %>%
  na.omit() %>% 
  dplyr::select(-Dev)

###########################################
# Everything below sucks and I hate it
# I'm bad and I should feel bad
###########################################

# Create subid field from subnum
# table is joined in creation of `dfWrite`
subjectIdLookup <- 
  tibble::rowid_to_column(as.data.frame(unique(df$subid)), "ID")
names(subjectIdLookup) <- c("subnum", "subid")

# Create participant info table from provided .xlsx
# table is joined in creation of `dfWrite`
participantInfoLookupTable <- readxl::read_excel("participant_info.xlsx") %>% 
  dplyr::select(`ID #`, Age, Gender)
names(participantInfoLookupTable) <- c("subid", "age", "sex")
participantInfoLookup <- participantInfoLookupTable %>%
  dplyr::mutate(
    sex = dplyr::if_else(sex == "F", 1, 0),
    age = round(age)
  )
participantInfoLookup$subid <- as.numeric(participantInfoLookup$subid)

# create assesment data info table from provided .xlsx
# table is joined in creation of `dfWrite`
assesmentDataLookupTable <- readxl::read_excel("assessment_data.xlsx")
assesmentDataLookupTable[assesmentDataLookupTable == "N/A"] <- NA
names(assesmentDataLookupTable)[1] <- "subid"



onlyComputerPeople <- dplyr::filter(df, robot == 0)
computerBlocks <- onlyComputerPeople %>% 
  dplyr::group_by(subid) %>% 
  dplyr::summarise(trials = n()) %>% 
  dplyr::mutate(blocks = trials / 24) %>% 
  dplyr::inner_join(onlyComputerPeople, by = "subid") %>%
  dplyr::group_by(subid) %>%
  dplyr::mutate(block = Vectorize(rep(1:blocks, times = 1, each = 24)), 
                trial = trial + ((block - 1) * 24)) %>% 
  select(-trials, -block, -blocks)

onlyRobotPeople <- dplyr::filter(df, robot == 1)
robotBlocks <- onlyRobotPeople %>% 
  dplyr::group_by(subid) %>% 
  dplyr::summarise(trials = n()) %>% 
  dplyr::mutate(blocks = trials / 12) %>% 
  dplyr::inner_join(onlyRobotPeople, by = "subid") %>%
  dplyr::group_by(subid) %>%
  dplyr::mutate(block = Vectorize(rep(1:blocks, times = 1, each = 12)), 
                trial = trial + ((block - 1) * 12)) %>% 
  select(-trials, -block, -blocks)


dfWrite <- 
  rbind.data.frame(robotBlocks, computerBlocks) %>%
  dplyr::inner_join(subjectIdLookup, by = "subid") %>%
  dplyr::inner_join(participantInfoLookup, by = "subid") %>%
  dplyr::left_join(assesmentDataLookupTable, by = "subid")

write.csv(dfWrite, file = "merged.csv", row.names = FALSE)

# rm(list = ls())
