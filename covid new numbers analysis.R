#### plot of new cases by age group
delta.cases <- covid.az[,c(1,11,13,15,17,19)]
colnames(delta.cases) <- c('Date', 'Under20', '20-44', '45-54', '55-64', 'Over65')
delta.cases.m <- melt(delta.cases, id.vars='Date', measure.vars=c('Under20', '20-44', '45-54', '55-64', 'Over65'), na.rm=T)
colnames(delta.cases.m)<-c('Date', 'Age.Group', 'Number.of.New.Cases')

ggplot(delta.cases.m, aes(x=Date, y=Number.of.New.Cases, color=Age.Group, fill=Age.Group))+
  # geom_point()+
  facet_wrap(~Age.Group)+
  geom_bar(stat='identity')+
  geom_line(aes(y=rollmean(Number.of.New.Cases,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Number of New Cases by Age Group - Arizona')+
  theme_bw()
ggsave(paste0("Number of New Cases by Age Group "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

#### plot of new hospitilizations by age group
delta.hosp <- covid.az[,c(1,33,35,37,39,41)]
colnames(delta.hosp) <- c('Date', 'Under20', '20-44', '45-54', '55-64', 'Over65')
delta.hosp<- delta.hosp[-c(16:17),] ####this line needs to be updated daily to exclude the data from 7-23 and 7-24
delta.hosp.m <- melt(delta.hosp, id.vars='Date', measure.vars=c('Under20', '20-44', '45-54', '55-64', 'Over65'), na.rm=T)
colnames(delta.hosp.m)<-c('Date', 'Age.Group', 'Number.of.New.Hosp')

ggplot(delta.hosp.m, aes(x=Date, y=Number.of.New.Hosp, color=Age.Group, fill=Age.Group))+
  # geom_point()+
  facet_wrap(~Age.Group)+
  geom_bar(stat='identity')+
  geom_line(aes(y=rollmean(Number.of.New.Hosp,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Number of New Hosp. by Age Group - Arizona')+
  theme_bw()
ggsave(paste0("Number of New Hospitalizations by Age Group "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))
#### plot of new Death by age group
delta.Deaths <- covid.az[,c(1,23,25,27,29,31)]
colnames(delta.Deaths) <- c('Date', 'Under20', '20-44', '45-54', '55-64', 'Over65')
delta.Deaths.m <- melt(delta.Deaths, id.vars='Date', measure.vars=c('Under20', '20-44', '45-54', '55-64', 'Over65'), na.rm=T)
colnames(delta.Deaths.m)<-c('Date', 'Age.Group', 'Number.of.New.Deaths')

ggplot(delta.Deaths.m, aes(x=Date, y=Number.of.New.Deaths, color=Age.Group, fill=Age.Group))+
  # geom_point()+
  facet_wrap(~Age.Group)+
  geom_bar(stat='identity')+
  geom_line(aes(y=rollmean(Number.of.New.Deaths,7,fill=NA)), linetype='twodash', color='red', size=1.5)+
  ggtitle('Number of New Deaths. by Age Group - Arizona')+
  theme_bw()
ggsave(paste0("Number of New Deaths by Age Group "
              , format(Sys.time(), "%Y-%m-%d")
              , ".png"))

