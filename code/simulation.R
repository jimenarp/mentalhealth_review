rm(list=ls())

 # simulation 

## t0

nt<-1025
meant<-.101
nc<-755
meanc<-.114

t<-rep(c(1),each=nt)
tdistress<-rep(c(0,1),times=c(nt-round(meant*nt),round(meant*nt)))
c<-rep(c(0),each=nc)
cdistress<-rep(c(0,1),times=c(nc-round(meanc*nc),round(meanc*nc)))

treatment<-cbind(t,tdistress)
control<-cbind(c,cdistress)

baseline<-rbind(treatment, control)
baseline<-as.data.frame(baseline)
names(baseline)<-c('treat','bas_distress')


## t0

nt<-1025
meant<-.09
nc<-755
meanc<-.112

t<-rep(c(1),each=nt)
tdistress<-rep(c(0,1),times=c(nt-round(meant*nt),round(meant*nt)))

c<-rep(c(0),each=nc)
cdistress<-rep(c(0,1),times=c(nc-round(meanc*nc),round(meanc*nc)))

treatment<-cbind(t,tdistress)
control<-cbind(c,cdistress)

endline<-rbind(treatment, control)
endline<-as.data.frame(endline)
names(endline)<-c('treat','end_distress')

sample<-baseline
sample$end_distress<-endline$end_distress

# regression

test<-lm(end_distress~ treat +bas_distress, data=sample)
stargazer(test, type='text')

### standarizing '

meanend<-mean(sample$end_distress[sample$treat==0])
meansd<-sd(sample$end_distress[sample$treat==0])

estimatedsd<-(meanend*(1-meanend)/nc)^0.5
realsd<-estimatedsd*nc^.5 ### we were estimatind standard error not deviation
(meanend*(1-meanend)/nc)^0.5

sample<- sample %>% 
  mutate(z_bas=(bas_distress-meanend)/meansd) %>%
  mutate(z_end=(end_distress-meanend)/meansd)

# regression

test<-lm(z_end~ treat +z_bas, data=sample)
stargazer(test, type='text')



