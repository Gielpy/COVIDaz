library(tidyverse)
library(zoo)

az.pop = 7189000
az.pop.U18 = 1617500
az.pop.18to65=4277500
az.pop.O65 = 1294000

covid.az <- read.csv('covid data.csv', header=T)
covid.az$Date<-as.Date(covid.az$Date, '%m/%d/%Y')

#### plot of new cases by day
ggplot(covid.az, aes(x=Date, y=New.Cases))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(New.Cases,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('New Cases by Day - Arizona')+
  # labs()+
  theme_bw()
ggsave(paste0("New Cases by day "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))
  
#### plot of new inpatient change by day
ggplot(covid.az, aes(x=Date, y=New.Inpatient.Change))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(New.Inpatient.Change,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('New Inpatient Change by Day - Arizona')+
  theme_bw()
ggsave(paste0("New Inpatient Change by day "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### PCR positivity
covid.az$percent.positive = (covid.az$New.Cases/covid.az$New.PCR)*100
ggplot(covid.az, aes(x=Date, y=percent.positive))+
  geom_line(color='#003153', size=1)+
  geom_line(aes(y=rollmean(percent.positive,7,fill=NA)), linetype='twodash', size=1.5, color='red')+
  ggtitle('PCR Percent Positive - Arizona')+
  theme_bw()
ggsave(paste0("PCR Percent Positive by day "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### new deaths by day
ggplot(covid.az, aes(x=Date, y=New.Deaths))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(New.Deaths, 21, fill=NA)), linetype='twodash', size=1.5, color='red')+
  ggtitle('New Deaths by Day - Arizona')+
  theme_bw()
ggsave(paste0("New Deaths by day "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### cases by age as a percent of total
# covid.az$Cases.Under20 <- as.numeric(covid.az$Cases.Under20)
# covid.az$Cases.20.44 <- as.numeric(covid.az$Cases.20.44)
# covid.az$Cases.45.54 <- as.numeric(covid.az$Cases.45.54)
# covid.az$Cases.55.64 <- as.numeric(covid.az$Cases.55.64)
# covid.az$Cases.Over65 <- as.numeric(covid.az$Cases.Over65)

covid.cases=NULL
covid.cases$CU20pT = (covid.az$Cases.Under20/covid.az$Positive.Tests)*100
covid.cases$C20.44pT = (covid.az$Cases.20.44/covid.az$Positive.Tests)*100
covid.cases$C45.54pT = (covid.az$Cases.45.54/covid.az$Positive.Tests)*100
covid.cases$C55.64pT = (covid.az$Cases.55.64/covid.az$Positive.Tests)*100
covid.cases$CO65pT = (covid.az$Cases.Over65/covid.az$Positive.Tests)*100
covid.cases$Date <- covid.az$Date
covid.cases <- data.frame(covid.cases)

library(reshape2)
covid.cases.m <- melt(covid.cases, id.vars='Date', measure.vars=c('CU20pT', 'C20.44pT', 'C45.54pT', 'C55.64pT', 'CO65pT'))

ggplot(covid.cases.m, aes(x=Date, y=value, fill=variable))+
  geom_bar(stat='identity')+
  ylab('Percentage of Cases by Age Group')+
  ggtitle('Proportion of Cases by Age - Arizona')+
  theme_bw()+
  # labs(fill='Age Group')
  scale_fill_discrete(name = "Age Group", labels = c("Under 20", "20-44", "45-54", '55-64', 'Over 65'))
ggsave(paste0("Proportion of Cases by Age "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### deaths by age as a percent of total
# covid.az$Deaths.Under20 <- as.numeric(covid.az$Deaths.Under20)
# covid.az$Deaths.20.44 <- as.numeric(covid.az$Deaths.20.44)
# covid.az$Deaths.45.54 <- as.numeric(covid.az$Deaths.45.54)
# covid.az$Deaths.55.64 <- as.numeric(covid.az$Deaths.55.64)
# covid.az$Deaths.Over65 <- as.numeric(covid.az$Deaths.Over65)

covid.Deaths=NULL
covid.Deaths$DU20pT = (covid.az$Deaths.Under20/covid.az$Deaths)*100
covid.Deaths$D20.44pT = (covid.az$Deaths.20.44/covid.az$Deaths)*100
covid.Deaths$D45.54pT = (covid.az$Deaths.45.54/covid.az$Deaths)*100
covid.Deaths$D55.64pT = (covid.az$Deaths.55.64/covid.az$Deaths)*100
covid.Deaths$DO65pT = (covid.az$Deaths.Over65/covid.az$Deaths)*100
covid.Deaths$Date <- covid.az$Date
covid.Deaths <- data.frame(covid.Deaths)

# library(reshape2)
covid.Deaths.m <- melt(covid.Deaths, id.vars='Date', measure.vars=c('DU20pT', 'D20.44pT', 'D45.54pT', 'D55.64pT', 'DO65pT'))

ggplot(covid.Deaths.m, aes(x=Date, y=value, fill=variable))+
  geom_bar(stat='identity')+
  ylab('Percentage of Deaths by Age Group')+
  theme_bw()+
  scale_fill_discrete(name = "Age Group", labels = c("Under 20", "20-44", "45-54", '55-64', 'Over 65'))
ggsave(paste0("Proportion of Deaths by Age Group "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### Hospitalizations by age as a percent of total
# covid.az$hosp.Under20 <- as.numeric(covid.az$hosp.Under20)
# covid.az$Hospitalizations.20.44 <- as.numeric(covid.az$Hospitalizations.20.44)
# covid.az$hosp.45.54 <- as.numeric(covid.az$hosp.45.54)
# covid.az$hosp.55.64 <- as.numeric(covid.az$hosp.55.64)
# covid.az$Hospitalizations.Over65 <- as.numeric(covid.az$Hospitalizations.Over65)

covid.hosp=NULL
covid.hosp$HU20pT = (covid.az$Hospitalizations.Under20/covid.az$Hospitalizations)*100
covid.hosp$H20.44pT = (covid.az$Hospitalizations.20.44/covid.az$Hospitalizations)*100
covid.hosp$H45.54pT = (covid.az$Hospitalizations.45.54/covid.az$Hospitalizations)*100
covid.hosp$H55.64pT = (covid.az$Hospitalizations.55.64/covid.az$Hospitalizations)*100
covid.hosp$HO65pT = (covid.az$Hospitalizations.Over65/covid.az$Hospitalizations)*100
covid.hosp$Date <- covid.az$Date
covid.hosp <- data.frame(covid.hosp)

# library(reshape2)
covid.hosp.m <- melt(covid.hosp, id.vars='Date', measure.vars=c('HU20pT', 'H20.44pT', 'H45.54pT', 'H55.64pT', 'HO65pT'))

ggplot(covid.hosp.m, aes(x=Date, y=value, fill=variable))+
  geom_bar(stat='identity')+
  ylab('Percentage of Hospitalizations by Age Group')+
  ggtitle('Proportion of Hospitalizations by Age - Arizona')+
  theme_bw()+
  # labs(fill='Age Group')
  scale_fill_discrete(name = "Age Group", labels = c("Under 20", "20-44", "45-54", '55-64', 'Over 65'))
ggsave(paste0("Proportion of Hospitalizations by Age Group "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

