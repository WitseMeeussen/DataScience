#source('DataLoad.R')

#first calculate the variance in temperature between departments

library(corrr)
library(reshape2)

ou <- binnenData %>%
  dplyr::filter(kastemperatuurklimaat< 10 & kastemperatuurklimaat> 0)

windsnelheiddata <- buitenData %>%
  select(date,time,windsnelheid,windrichting,buitentemperatuur,stralingssom)%>%
  dplyr::filter(windsnelheid> 0)%>%
  mutate(windrichting = factor(windrichting, levels=c('1','2','4','8','16','128','32','64'), labels=c('N','NE','E','SE','S','SW','W', 'NW')))



maxRaamstand <-  max(binnenData$luwezijderaamstand,na.rm = TRUE)
  
tempvariation <-binnenData %>%
  select(date,time,AVklimaat,doek,luwezijderaamstand,windzijderaamstand,kastemperatuurklimaat,assimilatiebelichting,AVklimaat,CO2,transport)%>%
  dplyr::filter(kastemperatuurklimaat> 0)%>%
  group_by(date,time)%>%
  summarise(TemperatuurDeviatie = sd(kastemperatuurklimaat),
            luwezijderaamstand = mean(luwezijderaamstand,na.rm = TRUE),
            doek = sd(doek,na.rm = TRUE),
            windzijderaamstand = sd(windzijderaamstand,na.rm = TRUE),
            AVklimaat = mean(AVklimaat,na.rm = TRUE),
            kastemperatuurklimaat = mean(kastemperatuurklimaat,na.rm = TRUE),
            assimilatiebelichting = sd(assimilatiebelichting,na.rm = TRUE),
            AVklimaatSD = sd(AVklimaat,na.rm = TRUE),
            CO2 = sd(CO2,na.rm=TRUE),
            transport = sd(transport,na.rm=TRUE))%>%
  full_join(windsnelheiddata,by = intersect(c('date','time'),c('date','time')))%>%
  dplyr::filter(!is.na(TemperatuurDeviatie))%>%
  dplyr::filter(!is.na(windsnelheid))%>%
  mutate(windInvloed = luwezijderaamstand*windsnelheid)

## temperatuurDeviatie tegenover invloed van wind

# scatter plot 
  
ggplot(data=tempvariation,mapping = aes(x = TemperatuurDeviatie, y=windInvloed)) +
  geom_point(alpha=0.1) +
  facet_wrap( ~ windrichting, nrow=2)

#globale corelatie
cor(tempvariation$TemperatuurDeviatie,tempvariation$windsnelheid)

filter<-tempvariation%>%
  filter(TemperatuurDeviatie > 4)
  

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
  labs(title = 'Corelation between variable and temperature deviation',y ='Variables',x ='Wind richtingen', fill = 'correlation')+
  geom_tile() + facet_wrap( ~ windrichting, nrow=1)

#calculating correlation between temperatureDeviation and windInvloed
corPlotData%>%
  dplyr::filter(Var2 == 'windInvloed')%>%
  head(8)

# correlation with plant data
plantData%>%
  filter(Verlichting != "LED")%>%
  group_by(date,time)%>%
  summarise(sapstroom=mean(sapstroom,na.rm = TRUE),
            diameter=mean(diameter,na.rm=TRUE))%>%
  right_join(tempvariation,by = intersect(c('date','time'),c('date','time')))%>%
  filter(!is.na(sapstroom))%>%
  filter(!is.na(diameter))%>%
  group_by(sapstroom)%>%
  select(-date,-time,-windrichting)%>%
  cor()%>%
  melt()%>%
  ggplot(aes(x=Var1, y=Var2, fill=value))+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_tile()





