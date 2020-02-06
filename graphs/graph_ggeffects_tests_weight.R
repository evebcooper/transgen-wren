library(ggeffects)
library(ggplot2)
######

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")

#weight graphs
library(ggplot2)
library(dplyr)
library(ggeffects)
library(prediction)

#put 9 and 10 year olds at 8 
offspring<-offspring %>%
  mutate(BageC=ifelse(Bage>9,9,Bage)) %>%
  mutate(SageC=ifelse(Sage>9,9,Sage))
#######################################################################################
#weight
#weight w/ RE
wGraph<-lmer(weight~Mage+BageC:EPb+BageC:WPb+SageC:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspring)
summary(wGraph)

#dataframe
weightDat<-offspring %>%
  select(Mage,BageC,Bage,SageC,lifespanB,lifespanS,lifespanB,lifespanM,Jincube,weight.age,pre1992,weight,EPb,
         cohort, mum, dad_bio,dad_soc,WPb)
weightDat<-na.omit(weightDat)

######################################################################################################################
#predictions
#ggemmeans
# WP appears NS
#intercept well above were it should be for all groups

pred.BEP<-ggemmeans(wGraph,terms=c("BageC"),type="fe", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.BWP<-ggemmeans(wGraph,terms=c("BageC"),type="fe",condition=c(WPb="1",EPb="0"))
pred.S<-ggemmeans(wGraph,terms=c("SageC"),type="fe",condition=c(EPb="1",WPb="0"))
pred.BEP<-pred.BEP %>%
  rename(pred.BEP=predicted,conf.lowBEP=conf.low,conf.highBEP=conf.high)%>%
  select(-group,-std.error)
pred.BWP<-pred.BWP %>%
  rename(pred.BWP=predicted,conf.lowBWP=conf.low,conf.highBWP=conf.high)%>%
  select(-group,-std.error)
pred.S<-pred.S %>%
  rename(pred.S=predicted,conf.lowS=conf.low,conf.highS=conf.high)%>%
  select(-group,-std.error)

pred.all<-left_join(pred.BEP,pred.BWP,by="x")
pred.all<-left_join(pred.all,pred.S,by="x")

#############################
#mean values in raw data
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(resid=resid(weightG)) %>%
  mutate(weightCor=weight-resid)
#summarise age raw data to 9 rows
weightDatWP <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(numBWP=n(),weightBWP=mean(weightCor))
weightDatEPb <- weightDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(numBEP=n(),weightBEP=mean(weightCor))
weightDatEPs <- weightDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(numSEP=n(),weightSEP=mean(weightCor))%>%
  rename(BageC=SageC)
pred.all<-left_join(pred.all,weightDatWP,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPb,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPs,by=c("x"="BageC"))



#weight - all
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"))+
  geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(mapping=aes(x=x,y=pred.BEP,colour="Germ-line"))+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"))+
  geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  geom_point(aes(x=x,y=weightSEP,colour="Environment"))+
  geom_point(aes(x=x,y=weightBEP,colour="Germ-line"))+
  geom_point(aes(x=x,y=weightBWP,colour="Germ-line & Environment"))+
  labs( x = "Father Age", y = "Offspring Growth",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  #scale_y_continuous(limits=c(0.95,1.04),breaks=c(0.96,0.98,1.00,1.02,1.04))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                          "Environment","Germ-line & Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                           "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                           "Environment","Germ-line & Environment")) 


#######################################################################
#predictions
#ggpredict
#type= 're' and 'fe' do not give prediction intervals
#type= sim just produces raw data
#type = sim can only take 3 values in terms, and then it produces estimates for each unique combos of those values
#did not attempt to graph the above but CIs appear very large

wGraph<-lmer(weight~Mage+BageC:EPb+BageC:WPb+SageC:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspring)
summary(wGraph)


pred.BEP<-ggpredict(wGraph,terms=c("BageC","Mage"),type="sim", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.BWP<-ggpredict(wGraph,terms=c("BageC","Mage","SageC","lifespanB","lifespanS","lifespanM","Jincube","pre1992",
                                   "weight.age"),type="sim",condition=c(WPb=1,EPb=0))
pred.S<-ggpredict(wGraph,terms=c("SageC"),type="sim",condition=c(EPb="1",WPb="0"))
pred.BEP<-pred.BEP %>%
  rename(pred.BEP=predicted,conf.lowBEP=conf.low,conf.highBEP=conf.high)%>%
  select(-group)
pred.BWP<-pred.BWP %>%
  rename(pred.BWP=predicted,conf.lowBWP=conf.low,conf.highBWP=conf.high)%>%
  select(-group)
pred.S<-pred.S %>%
  rename(pred.S=predicted,conf.lowS=conf.low,conf.highS=conf.high)%>%
  select(-group)

pred.all<-left_join(pred.BEP,pred.BWP,by="x")
pred.all<-left_join(pred.all,pred.S,by="x")

#############################
#mean values in raw data
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(resid=resid(weightG)) %>%
  mutate(weightCor=weight-resid)
#summarise age raw data to 9 rows
weightDatWP <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(numBWP=n(),weightBWP=mean(weightCor))
weightDatEPb <- weightDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(numBEP=n(),weightBEP=mean(weightCor))
weightDatEPs <- weightDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(numSEP=n(),weightSEP=mean(weightCor))%>%
  rename(BageC=SageC)
pred.all<-left_join(pred.all,weightDatWP,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPb,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPs,by=c("x"="BageC"))



#weight - all
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"))+
  #geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(mapping=aes(x=x,y=pred.BEP,colour="Germ-line"))+
  #geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"))+
  #geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  geom_point(aes(x=x,y=weightSEP,colour="Environment"))+
  geom_point(aes(x=x,y=weightBEP,colour="Germ-line"))+
  geom_point(aes(x=x,y=weightBWP,colour="Germ-line & Environment"))+
  labs( x = "Father Age", y = "Offspring Growth",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  #scale_y_continuous(limits=c(0.95,1.04),breaks=c(0.96,0.98,1.00,1.02,1.04))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                          "Environment","Germ-line & Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                           "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                           "Environment","Germ-line & Environment"))


######################################################################################################################
#predictions
#ggaverage
#CIs always are NA
#you can add up to 3 terms


pred.BEP<-ggaverage(wGraph,terms=c("BageC"),type="fe", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.BWP<-ggaverage(wGraph,terms=c("BageC"),type="re",condition=c(WPb="1",EPb="0"))
pred.S<-ggaverage(wGraph,terms=c("SageC"),type="fe",condition=c(EPb="1",WPb="0"))
pred.BEP<-pred.BEP %>%
  rename(pred.BEP=predicted,conf.lowBEP=conf.low,conf.highBEP=conf.high)%>%
  select(-group,-std.error)
pred.BWP<-pred.BWP %>%
  rename(pred.BWP=predicted,conf.lowBWP=conf.low,conf.highBWP=conf.high)%>%
  select(-group,-std.error)
pred.S<-pred.S %>%
  rename(pred.S=predicted,conf.lowS=conf.low,conf.highS=conf.high)%>%
  select(-group,-std.error)

pred.all<-left_join(pred.BEP,pred.BWP,by="x")
pred.all<-left_join(pred.all,pred.S,by="x")

#############################
#mean values in raw data
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(resid=resid(weightG)) %>%
  mutate(weightCor=weight-resid)
#summarise age raw data to 9 rows
weightDatWP <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(numBWP=n(),weightBWP=mean(weightCor))
weightDatEPb <- weightDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(numBEP=n(),weightBEP=mean(weightCor))
weightDatEPs <- weightDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(numSEP=n(),weightSEP=mean(weightCor))%>%
  rename(BageC=SageC)
pred.all<-left_join(pred.all,weightDatWP,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPb,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPs,by=c("x"="BageC"))



#weight - all
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"))+
  geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(mapping=aes(x=x,y=pred.BEP,colour="Germ-line"))+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"))+
  geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  geom_point(aes(x=x,y=weightSEP,colour="Environment"))+
  geom_point(aes(x=x,y=weightBEP,colour="Germ-line"))+
  geom_point(aes(x=x,y=weightBWP,colour="Germ-line & Environment"))+
  labs( x = "Father Age", y = "Offspring Growth",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  #scale_y_continuous(limits=c(0.95,1.04),breaks=c(0.96,0.98,1.00,1.02,1.04))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                          "Environment","Germ-line & Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                           "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                           "Environment","Germ-line & Environment")) 


######################################################################################################################
#predictions
#ggeffect
#condition does not appear to be working
#when adding EPb to the terms, both groups improve with BageC
#did not run all the way through but it looks like terms will be NS

pred.BEP<-ggeffect(wGraph,terms=c("BageC"),type="fe", condition=c(EPb=1,WPb=0))  #%>%plot()
pred.BWP<-ggeffect(wGraph,terms=c("BageC","EPb"),type="re",condition=c(Mage=1))
pred.S<-ggeffect(wGraph,terms=c("SageC"),type="fe",condition=c(EPb="1",WPb="0"))
pred.BEP<-pred.BEP %>%
  rename(pred.BEP=predicted,conf.lowBEP=conf.low,conf.highBEP=conf.high)%>%
  select(-group,-std.error)
pred.BWP<-pred.BWP %>%
  rename(pred.BWP=predicted,conf.lowBWP=conf.low,conf.highBWP=conf.high)%>%
  select(-group,-std.error)
pred.S<-pred.S %>%
  rename(pred.S=predicted,conf.lowS=conf.low,conf.highS=conf.high)%>%
  select(-group,-std.error)

pred.all<-left_join(pred.BEP,pred.BWP,by="x")
pred.all<-left_join(pred.all,pred.S,by="x")

#############################
#mean values in raw data
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(resid=resid(weightG)) %>%
  mutate(weightCor=weight-resid)
#summarise age raw data to 9 rows
weightDatWP <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(numBWP=n(),weightBWP=mean(weightCor))
weightDatEPb <- weightDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(numBEP=n(),weightBEP=mean(weightCor))
weightDatEPs <- weightDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(numSEP=n(),weightSEP=mean(weightCor))%>%
  rename(BageC=SageC)
pred.all<-left_join(pred.all,weightDatWP,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPb,by=c("x"="BageC"))
pred.all<-left_join(pred.all,weightDatEPs,by=c("x"="BageC"))



#weight - all
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"))+
  geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(mapping=aes(x=x,y=pred.BEP,colour="Germ-line"))+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"))+
  geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  geom_point(aes(x=x,y=weightSEP,colour="Environment"))+
  geom_point(aes(x=x,y=weightBEP,colour="Germ-line"))+
  geom_point(aes(x=x,y=weightBWP,colour="Germ-line & Environment"))+
  labs( x = "Father Age", y = "Offspring Growth",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  #scale_y_continuous(limits=c(0.95,1.04),breaks=c(0.96,0.98,1.00,1.02,1.04))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                          "Environment","Germ-line & Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                           "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                           "Environment","Germ-line & Environment")) 





