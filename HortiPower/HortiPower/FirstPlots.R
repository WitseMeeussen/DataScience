source('DataLoad.R')

#first calculate the variance in temperature between departments

install.packages("corrr")
library(corrr)

ou <- binnenData %>%
  dplyr::filter(kastemperatuurklimaat< 10 & kastemperatuurklimaat> 0)

windsnelheiddata <- buitenData %>%
  select(date,time,windsnelheid)%>%
  dplyr::filter(windsnelheid> 0)


maxRaamstand <-  max(binnenData$luwezijderaamstand,na.rm = TRUE)
tempvariation <-binnenData %>%
  dplyr::filter(kastemperatuurklimaat> 0)%>%
  group_by(date,time)%>%
  mutate(TemperatuurDeviatie = sd(kastemperatuurklimaat))%>%
  mutate(raaminvloet = sum(luwezijderaamstand,na.rm = TRUE)/maxRaamstand)%>%
  select(date,time,TemperatuurDeviatie,windzijderaamstand,luwezijderaamstand,raaminvloet)%>%
  full_join(windsnelheiddata,by = intersect(c('date','time'),c('date','time')))

ggplot(data=tempvariation) +
  geom_point(mapping = aes(x = TemperatuurDeviatie, y=windsnelheid*raaminvloet),alpha=0.1)