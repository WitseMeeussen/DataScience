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
  if(length(dirs)==1) return (recursive.ReadDepartment(files))
  else return (bind_rows(recursive.ReadDepartment(files),recursive.ReadBinnenData(dirs[-1])))
}

recursive.ReadDir <- function(files){
  file <-  read_delim(files[1],col_names=TRUE, col_types = NULL)
  if(length(files) ==1)return (file[,-4]) 
  else return (full_join(file[,-4],recursive.ReadDepartment(files[-1]),by = intersect(c('date','time'),c('date','time'))))
  
}

#Reads the directories containing the measurements per department
binnenDirs <- dir("../Data", pattern="Binnenmetingen(...)", full.names = TRUE)
binnenData <- recursive.ReadBinnenData(binnenDirs)%>%
  mutate(date= as.Date(date, format= "%d-%m-%Y"))

# excludes a non defining column group
buitenData <- recursive.ReadDir(list.files("../Data/Buitenmetingen", full.names = TRUE))[,-9]%>%
  mutate(date= as.Date(date, format= "%d-%m-%Y"))
colTypes <- list(col_character(),col_time(),col_character(),col_character(),col_time(),col_character())
colNames <- c('dateS', 'timeS','sapstroom','dateD','timeD','diameter')
#gelijktrekken op tijd
plantDataLED <- read_table("../Data/Portento Belichting LED 2020.txt",col_names= colNames, col_types = colTypes, skip = 1)%>%
  mutate(Verlichting = "LED")
plantDataSonT <- read_table("../Data/Portento Belichting SonT 2020.txt",col_names=colNames, col_types = colTypes, skip = 1)%>%
  mutate(Verlichting = "SonT")

commaNumberTodbl<- function(number){
  before <- as.double(sub(',','.',number))
}

reorderPlantData<-function(plantData){
  sap <- plantData[1:3]%>%
    mutate(date = as.Date(dateS, format= "%d/%m/%Y"))%>%
    select(-dateS)%>%
    mutate(sapstroom = map_dbl(sapstroom, commaNumberTodbl))%>%
    rename(time = timeS)
  diameter <- plantData[4:7]%>%
    mutate(date = as.Date(dateD, format= "%d/%m/%Y"))%>%
    select(-dateD)%>%
    mutate(diameter = map_dbl(diameter, commaNumberTodbl))%>%
    rename(time = timeD)
  return(left_join(sap,diameter,by = intersect(c('date','time'),c('date','time'))))
}

plantData <- bind_rows(reorderPlantData(plantDataLED),reorderPlantData(plantDataSonT))

