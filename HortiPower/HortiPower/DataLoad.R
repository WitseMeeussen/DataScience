library(tidyverse)
binnenDirs <- dir("../Data", pattern="Binnenmetingen(...)", full.names = TRUE)

data <- read_delim("../Data/Buitenmetingen/Buiten_RVmeteo.txt",col_names=TRUE, col_types = NULL)

# trying to use recursion to load the files but the nested loop works but is not verry readable
readDir <- function(direction){
  if(grepl(pattern=".txt", x=i)){
    file = read_table(dir)
  }else{
    
  }
}
# loops true the directions and tries to add the collumn. collumn not exact length it is screwed
binnenDirs
for (i in binnenDirs){
  files <- list.files(i, full.names = TRUE)
  for(j in files){
    file <- read_delim(j,col_names=TRUE, col_types = NULL)
    data %>% add_column(k = file[[2]])
  }
}



data