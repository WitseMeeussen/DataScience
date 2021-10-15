library(tidyverse)
library(tibble)
binnenDirs <- dir("../Data", pattern="Binnenmetingen(...)", full.names = TRUE)


# test
#data2 <- read_delim("../Data/Binnenmetingen 1A/File1A_assimilatiebelichting.txt",col_names=TRUE, col_types = NULL, delim=";")
#data3 <- read_delim("../Data/Binnenmetingen 1A/File1A_AVklimaat.txt",col_names=TRUE, col_types = NULL, delim=";")
#data4 <- merge(data2,data3,by = intersect(c('date','time'),c('date','time')),incomparables = NULL)
#data4
# trying to use recursion to load the files but the nested loop works but is not verry readable
# loops true the directions and tries to add the collumn. collumn not exact length it is screwed
dat <- NULL
fullBinnenData <- NULL
for (i in binnenDirs){
  files <- list.files(i, full.names = TRUE)
  groupNumber <- gsub('^(.*)\\s','',i)
  for(j in files){
    file <- read_delim(j,col_names=TRUE, col_types = NULL)
    if(is.null(dat)){
      file[,4] <- groupNumber
      dat <- file
    } else{
      noGroup <-file[,-4]
      dat <- full_join(dat,noGroup,by = intersect(c('date','time'),c('date','time')))
    }
    
  }
  if(is.null(fullBinnenData)){
    fullBinnenData <- dat
  }else{
    fullBinnenData <- bind_rows(fullBinnenData,dat)
  }
}

# tables 
