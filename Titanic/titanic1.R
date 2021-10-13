library(tidyverse)

files <- lapply(list.files(system.file('extdata', package = 'my_package'), full.names = TRUE), read.csv)