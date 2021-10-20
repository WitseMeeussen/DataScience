library(tidyverse)
library(tibble)

#  functions
# returns a tibble that contains information of all the files joined (used to read Departments)
recursive.ReadDepartment <- function(files){
  file <-  read_delim(files[1],col_names=TRUE, col_types = NULL)
  if(length(files) ==1){
    file[,4] <- gsub('^(.*)\\s|/(.*)$','',files[1])
    return (file)
  } 
  else return (full_join(file[,-4],recursive.ReadDepartment(files[-1]),by = intersect(c('date','time'),c('date','time'))))
  
}


#returns a tibble containing every department info added together row wise
recursive.ReadBinnenData <- function(dirs){
  files <- list.files(dirs[1], full.names = TRUE)
  if(length(dirs)==1) return (recursive.ReadDepartment(files)[['group']])
  else return (bind_rows(recursive.ReadDepartment(files),recursive.ReadBinnenData(dirs[-1])))
}

recursive.ReadDir <- function(files){
  file <-  read_delim(files[1],col_names=TRUE, col_types = NULL)
  if(length(files) ==1)return (file[,-4]) 
  else return (full_join(file[,-4],recursive.ReadDepartment(files[-1]),by = intersect(c('date','time'),c('date','time'))))
  
}

#Reads the directories containing the measurements per department
binnenDirs <- dir("../Data", pattern="Binnenmetingen(...)", full.names = TRUE)
binnenData <- recursive.ReadBinnenData(binnenDirs)

# excludes a non defining column group
buitenData <- recursive.ReadDir(list.files("../Data/Buitenmetingen", full.names = TRUE))[,-9]

#gelijktrekken op tijd
plantDataLED <- read_table("../Data/Portento Belichting LED 2020.txt",col_names=c('dateS', 'timeS','sapstroom','dateD','timeD','diameter'), col_types = NULL, skip = 1)
plantDataSonT <- read_table("../Data/Portento Belichting SonT 2020.txt",col_names=c('dateS', 'timeS','sapstroom','dateD','timeD','diameter'), col_types = NULL, skip = 1)
#plantDataLED[1:3]
#plantDataLED[4:6]

#dataLed <- left_join(plantDataLED[1:3],plantDataLED[4:6],by = intersect(c('dateS'),c('dateD')))
