source('DataLoad.R')

#first calculate the variance in temperature between departments

install.packages("corrr")
library(corrr)

ou <- binnenData %>%
  dplyr::filter(kastemperatuurklimaat< 10 & kastemperatuurklimaat> 0)

windsnelheiddata <- buitenData %>%
  select(date,time,windsnelheid)%>%
  dplyr::filter(windsnelheid> 0)

tempvariation <-binnenData %>%
  dplyr::filter(kastemperatuurklimaat> 0)%>%
  group_by(date,time)%>%
  mutate(sd = sd(kastemperatuurklimaat))%>%
  mutate(wind = sum(windzijderaamstand))%>%
  select(date,time,sd,windzijderaamstand,luwezijderaamstand,wind)%>%
  full_join(windsnelheiddata,by = intersect(c('date','time'),c('date','time')))

ggplot(data=tempvariation) +
  geom_point(mapping = aes(x = sd, y=windsnelheid*wind,alpha=0.1,))