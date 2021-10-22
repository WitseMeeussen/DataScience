library('tidyverse')
# read_csv reads it as tibble
train <- read_csv('train.csv')
test <- read_csv('test.csv')

# binds the data because we dont need test data
full  <- bind_rows(train, test)



#functie extracting pronouns
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# regular expression \\. is the value of a point in the string
full$Surname <- gsub(',(.*)', '', full$Name)
full$Name <- gsub('(.*)\\.', '', full$Name)

#counting amount of surnames
full %>%
  select(Surname)%>%
  group_by(Surname)%>%
  count(Surname)%>%
  arrange(desc(n))

# changing the value of survived to distinct values instead of continues
full%>%
  filter(!is.na(Survived))%>%
  mutate(SurvivedBool = dplyr::if_else(Survived==1,'true','false'))%>%
  ggplot() +
    geom_bar(mapping = aes(x=Pclass,fill=SurvivedBool),position="fill")