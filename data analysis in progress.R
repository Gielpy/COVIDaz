covid.impact = NULL
covid.impact$U18.approx <- covid.az$Cases.Under20
covid.impact$f18t65 <- covid.az$Cases.20.44 + covid.az$Cases.45.54 + covid.az$Cases.55.64
covid.impact$O65 <- covid.az$Cases.Over65
covid.impact$Date <- covid.az$Date

covid.impact <- data.frame(covid.impact)

covid.impact$U18.percent <- (covid.impact$U18.approx/az.pop.U18)*100
covid.impact$f18t65.percent <- (covid.impact$f18t65/az.pop.18to65)*100
covid.impact$O65.percent <- (covid.impact$O65/az.pop.O65)*100

covid.impact.m <- melt(covid.impact, id.vars='Date', measure.vars=c('U18.percent', 'f18t65.percent', 'O65.percent'))

ggplot(covid.impact.m, aes(x=Date, y=value, fill=variable, color=variable))+
  geom_line()+
  geom_point()+
  ylab('Percentage of Population Testing Positive')+
  ggtitle('Percentage of Population testing Positive by Age - Arizona')+
  theme_bw()+
  # labs(fill='Age Group')
  scale_fill_discrete(name = "Age Group", labels = c("Under 18 (Approx.)", "18-65", 'Over 65'))
ggsave(paste0("Percentage of Population Testing Positive "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### percentage of cases hospitalized
cases.hosp=NULL
cases.hosp$HU20pT = (covid.az$Hospitilizations.Under20/covid.az$Hospitilizations)*100
cases.hosp$H20.44pT = (covid.az$Hospitilizations.20.44/covid.az$Hospitilizations)*100
cases.hosp$H45.54pT = (covid.az$Hospitilizations.45.54/covid.az$Hospitilizations)*100
cases.hosp$H55.64pT = (covid.az$Hospitilizations.55.64/covid.az$Hospitilizations)*100
cases.hosp$HO65pT = (covid.az$Hospitilizations.Over65/covid.az$Hospitilizations)*100
cases.hosp$Date <- covid.az$Date
cases.hosp <- data.frame(cases.hosp)

# library(reshape2)
cases.hosp.m <- melt(cases.hosp, id.vars='Date', measure.vars=c('HU20pT', 'H20.44pT', 'H45.54pT', 'H55.64pT', 'HO65pT'))

ggplot(cases.hosp.m, aes(x=Date, y=value, fill=variable, color=variable))+
  geom_line()+
  geom_point()+
  ylab('Percentage')+
  ggtitle('Proportion of Cases Hospitalized by Age - Arizona')+
  theme_bw()+
  # labs(fill='Age Group')
  scale_fill_discrete(name = "Age Group", labels = c("Under 20", "20-44", "45-54", '55-64', 'Over 65'))



#### plot of hospitalizations by day rolling average
covid.az.m <- melt(covid.az, id.vars='Date', measure.vars = c('Hospitilizations.Under20', 'Hospitilizations.20.44', 'Hospitilizations.45.54',
                                                              'Hospitilizations.55.64', 'Hospitilizations.Over65'))


ggplot(covid.az.m, aes(x=Date, y=value, color=variable))+
  geom_point()+
  # geom_line(aes(y=rollmean(value,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  # ggtitle('New Cases by Day - Arizona')+
  theme_bw()



#### plotting hospitalizations and new cases
ggplot()+
  geom_line(data=covid.az, aes(x=Date, y=rollmean(Hospitalized.Currently,7,fill=NA), color='#003153'), size=1)+
  geom_line(data=covid.az, aes(x=Date, y=rollmean(New.Cases,7,fill=NA), color='red'), size=1)+
  # geom_line(data=covid.az, aes(x=Date, y=rollmean(Positive.Tests,7,fill=NA), color='black'), size=1)+
  geom_line(data=covid.az, aes(x=Date, y=rollmean(New.Deaths, 7, fill=NA), color='green'), size=1)+
  ylab('Current Hospitalizations / New Cases / New Deaths')+
  scale_color_discrete(name = "Data series", labels = c("Hospitalized.Currently", "New Deaths", "New Cases"))+
  ggtitle('Tracking Hospitalizations, New Cases, and New Deaths')+
  theme_bw()
ggsave(paste0("Tracking Hospitilizations, New Cases, and New Deaths "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### plotting effects by age group
test <- covid.az[1,]
test.cases.m <- melt(test, id.vars='Date', measure.vars=c('Cases.Under20','Cases.20.44', 'Cases.45.54', 'Cases.55.64', 'Cases.Over65'))
test.Deaths.m <- melt(test, id.vars='Date', measure.vars=c('Deaths.Under20','Deaths.20.44', 'Deaths.45.54', 'Deaths.55.64', 'Deaths.Over65'))
test.Hospitalizations.m <- melt(test, id.vars='Date', measure.vars=c('Hospitilizations.Under20','Hospitilizations.20.44', 'Hospitilizations.45.54', 'Hospitilizations.55.64', 'Hospitilizations.Over65'))
colnames(test.cases.m)<- c('Date', 'Age Group', 'Cases')
colnames(test.Deaths.m)<- c('Date', 'Age Group', 'Deaths')
colnames(test.Hospitalizations.m)<- c('Date', 'Age Group', 'Hospitalizations')

test.full <- cbind(test.cases.m, test.Hospitalizations.m[,3], test.Deaths.m[,3])
colnames(test.full) <- c('Date', 'Age Group', 'Cases', 'Hospitalizations', 'Deaths')

levels(test.full$`Age Group`)[1]<-'Under 20'
levels(test.full$`Age Group`)[2]<-'20 to 44'
levels(test.full$`Age Group`)[3]<-'45 to 54'
levels(test.full$`Age Group`)[4]<-'55 to 64'
levels(test.full$`Age Group`)[5]<-'Over 65'

test.full.m <- melt(test.full, id.vars='Age Group', measure.vars=c('Cases', 'Hospitalizations', 'Deaths'))

ggplot(test.full.m, aes(x=`Age Group`, y=value, color=variable, fill=variable))+
  geom_bar(stat='identity', position=position_dodge())+
  ylab('Number')+
  ggtitle('Breakdown by Age Group - Arizona')+
  theme_bw()
ggsave(paste0("Breakdown by Age Group "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))



info.by.date <- covid.az[,c(1,48:51)]
# info.by.date$PCR.Date.of.Test<- as.numeric(info.by.date$PCR.Date.of.Test)
ggplot(info.by.date, aes(x=Date, y=Cases.Date.of.Administration))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(Cases.Date.of.Administration,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Cases by Date of Administration - Arizona')+
  # labs()+
  theme_bw()

ggplot(info.by.date, aes(x=Date, y=PCR.Date.of.Test))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(PCR.Date.of.Test,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('PCR Tests by Date of Administration - Arizona')+
  # labs()+
  theme_bw()

ggplot(info.by.date, aes(x=Date, y=delta.Cases.date))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(delta.Cases.date,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Difference in Cases by Date of Administration - Arizona')+
  # labs()+
  theme_bw()

ggplot(info.by.date, aes(x=Date, y=delta.PCR.date))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(delta.PCR.date,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Difference in Tests by Date of Administration - Arizona')+
  # labs()+
  theme_bw()

