ggplot(covid.az, aes(x=Date, y=New.Cases))+
  geom_bar(stat='identity', color='white', fill='#003153')+
  geom_line(aes(y=rollmean(New.Cases,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  geom_line(aes(y=rollmean(New.PCR,7,fill=NA)), linetype='twodash', color='green', size=1.5)+
  ggtitle('New Cases by Day - Arizona')+
  # labs()+
  theme_bw()

positivity = NULL
covid.az$positivity <- as.data.frame(covid.az$Cases.Date.of.Administration/covid.az$PCR.Date.of.Test)*100

p<-ggplot(covid.az, aes(x=Date))+
  geom_line(aes(y=rollmean(Cases.Date.of.Administration,7,fill=NA)), linetype='twodash', color='blue', size=1.5)+
  geom_line(aes(y=rollmean(PCR.Date.of.Test,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Comparing PCR Tests and Cases - Arizona')+
  # labs()+
  theme_bw()

q<-ggplot(covid.az, aes(x=Date))+
  geom_line(aes(y=rollmean(positivity,7,fill=NA)), linetype='twodash', color='green', size=1.5)+
  ggtitle('Positivity - Arizona')+
  # labs()+
  theme_bw()
require(gridExtra)
grid.arrange(p, q, ncol=2)
  