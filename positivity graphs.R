ggplot(covid.az, aes(x=Date, y=New.Cases))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(New.Cases,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  geom_line(aes(y=rollmean(New.PCR,7,fill=NA)), linetype='twodash', color='green', size=1.5)+
  ggtitle('New Cases by Day - Arizona')+
  # labs()+
  theme_bw()

positivity = NULL
covid.az$positivity <- as.data.frame(covid.az$Cases.Date.of.Administration/covid.az$PCR.Date.of.Test)*100

p<-ggplot(covid.az[-c(1:7),], aes(x=Date))+
  geom_line(aes(y=rollmean(Cases.Date.of.Administration,7,fill=NA)), linetype='twodash', color='blue', size=1.5)+
  geom_line(aes(y=rollmean(PCR.Date.of.Test,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Comparing PCR Tests and Cases - Arizona')+
  # labs()+
  theme_bw()

q<-ggplot(covid.az[-c(1:7),], aes(x=Date))+
  geom_line(aes(y=rollmean(positivity,7,fill=NA)), linetype='twodash', color='green', size=1.5)+
  ggtitle('Positivity - Arizona')+
  # labs()+
  theme_bw()
require(gridExtra)
grid.arrange(p, q, ncol=2)
ggsave(paste0("Tests, Cases, and Positivity "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

ggplot(covid.az, aes(x=Date))+
  geom_line(aes(y=rollmean(New.Inpatient.Change,7,fill=NA)), linetype='solid', color='#003153', size=1.5)+
  theme_bw()


rolling.cases=NULL
rolling.cases$Date <- covid.az$Date
rolling.cases$under20 <- covid.az$delta.Cases.Under20 / (covid.az$delta.Positives/1000)
rolling.cases$c20.44 <- covid.az$delta.Cases.20.44 / (covid.az$delta.Positives/1000)
rolling.cases$c45.54 <- covid.az$delta.Cases.45.54 / (covid.az$delta.Positives/1000)
rolling.cases$c55.64 <- covid.az$delta.Cases.55.64 / (covid.az$delta.Positives/1000)
rolling.cases$over65 <- covid.az$delta.Cases.Over65 / (covid.az$delta.Positives/1000)
rolling.cases <- data.frame(rolling.cases)

ggplot(rolling.cases, aes(x=Date))+
  geom_line(aes(y=rollmean(under20,7,fill=NA), color='Under 20'), linetype='solid',  size=1.5)+
  geom_line(aes(y=rollmean(c20.44,7,fill=NA), color='20 - 44'), linetype='solid',  size=1.5)+
  geom_line(aes(y=rollmean(c45.54,7,fill=NA), color='45 - 54'), linetype='solid',  size=1.5)+
  geom_line(aes(y=rollmean(c55.64,7,fill=NA), color='55 - 64'), linetype='solid', size=1.5)+
  geom_line(aes(y=rollmean(over65,7,fill=NA), color='Over 65'), linetype='solid',  size=1.5)+
  ggtitle('Cases per 1,000 Positives by Age Group - Arizona')+
  scale_color_manual(values=c('Under 20' = '#DF536B','20 - 44' = '#61d04f','45 - 54' = '#2297e6','55 - 64' = '#28e2e5',
    'Over 65' = '#cd0bbc'))+
  labs(color='Age Group')+
  ylab('Cases/1,000 Positive Tests')+
  theme_bw()


rolling.cases=NULL
rolling.cases$Date <- covid.az$Date
rolling.cases$under20 <- covid.az$delta.Cases.Under20 / (covid.az$delta.Tests/1000)
rolling.cases$c20.44 <- covid.az$delta.Cases.20.44 / (covid.az$delta.Tests/1000)
rolling.cases$c45.54 <- covid.az$delta.Cases.45.54 / (covid.az$delta.Tests/1000)
rolling.cases$c55.64 <- covid.az$delta.Cases.55.64 / (covid.az$delta.Tests/1000)
rolling.cases$over65 <- covid.az$delta.Cases.Over65 / (covid.az$delta.Tests/1000)
rolling.cases <- data.frame(rolling.cases)

ggplot(rolling.cases, aes(x=Date))+
  geom_line(aes(y=rollmean(under20,7,fill=NA), color='Under 20'), linetype='solid',  size=1.5)+
  geom_line(aes(y=rollmean(c20.44,7,fill=NA), color='20 - 44'), linetype='solid',  size=1.5)+
  geom_line(aes(y=rollmean(c45.54,7,fill=NA), color='45 - 54'), linetype='solid',  size=1.5)+
  geom_line(aes(y=rollmean(c55.64,7,fill=NA), color='55 - 64'), linetype='solid', size=1.5)+
  geom_line(aes(y=rollmean(over65,7,fill=NA), color='Over 65'), linetype='solid',  size=1.5)+
  ggtitle('Cases per 1,000 Tests by Age Group - Arizona')+
  scale_color_manual(values=c('Under 20' = '#DF536B','20 - 44' = '#61d04f','45 - 54' = '#2297e6','55 - 64' = '#28e2e5',
                              'Over 65' = '#cd0bbc'))+
  labs(color='Age Group')+
  ylab('Cases/1,000 Tests')+
  ylim(0,200)+
  theme_bw()
