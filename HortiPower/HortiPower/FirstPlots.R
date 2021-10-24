source('DataLoad.R')

#first calculate the variance in temperature between departments

install.packages("reshape2")
library(corrr)
library(reshape2)

ou <- binnenData %>%
  dplyr::filter(kastemperatuurklimaat< 10 & kastemperatuurklimaat> 0)

windsnelheiddata <- buitenData %>%
  select(date,time,windsnelheid,windrichting,buitentemperatuur,stralingssom)%>%
  dplyr::filter(windsnelheid> 0)%>%
  mutate(windrichting = factor(windrichting, levels=c('1','2','4','8','16','32','64'), labels=c('N','NE','E','SE','SW','W', 'NW')))



maxRaamstand <-  max(binnenData$luwezijderaamstand,na.rm = TRUE)
tempvariation <-binnenData %>%
  dplyr::filter(kastemperatuurklimaat> 0)%>%
  group_by(date,time)%>%
  mutate(TemperatuurDeviatie = sd(kastemperatuurklimaat))%>%
  mutate(raaminvloet = sum(luwezijderaamstand,na.rm = TRUE)/maxRaamstand)%>%
  select(-group,-assimilatiebelichting,-ventilatoren)%>%
  full_join(windsnelheiddata,by = intersect(c('date','time'),c('date','time')))%>%
  dplyr::filter(!is.na(TemperatuurDeviatie))%>%
  dplyr::filter(!is.na(windsnelheid))%>%
  mutate(WindInvloed = raaminvloet*windsnelheid)

## temperatuurDeviatie tegenover invloed van wind

# scatter plot 

ggplot(data=tempvariation) +
  geom_point(mapping = aes(x = TemperatuurDeviatie, y=windsnelheid*raaminvloet),alpha=0.1)

#globale corelatie
cor(tempvariation$TemperatuurDeviatie,tempvariation$windsnelheid)
  

getCorrelationsWindrichting <- function(richting){
  return(tempvariation%>%
    filter(windrichting == richting)%>%
    group_by(TemperatuurDeviatie)%>%
    select(-date,-time,-windrichting)%>%
    cor()%>%
    melt()%>%
    filter(Var1 == 'TemperatuurDeviatie')%>%
    mutate(windrichting= richting))
}

recursive.getWindrichtingen <- function(richtingen){
  if(length(richtingen)==1) return(getCorrelationsWindrichting(richtingen[1]))
  else{
    return(bind_rows(getCorrelationsWindrichting(richtingen[1]),recursive.getWindrichtingen(richtingen[-1])))
  }
}

corPlotData <- recursive.getWindrichtingen(levels(windsnelheiddata$windrichting))

# shows a plot with correlation of temperatuurdeviatie faceted by windrichting
recursive.getWindrichtingen(levels(windsnelheiddata$windrichting))%>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + facet_wrap( ~ windrichting, nrow=1)

#calculating correlation between temperatureDeviation and windInvloed
corPlotData%>%
  dplyr::filter(windrichting == 'SE')%>%
  dplyr::filter(Var2 == 'WindInvloed')%>%
  head()
