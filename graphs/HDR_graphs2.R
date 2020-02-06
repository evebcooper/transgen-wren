library(ggeffects)
library(ggplot2)
######
#offspring survival
pred.BEP<-ggemmeans(inde.sep1,terms=c("Bage"),type="fe", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.BWP<-ggemmeans(inde.sep1,terms=c("Bage"),type="fe",condition=c(WPb="1",EPb="0"))
pred.S<-ggemmeans(inde.sep1,terms=c("Sage"),type="fe",condition=c(EPb="1",WPb="0"))
pred.S<-filter(pred.S,x<11)
pred.BEP<-pred.BEP %>%
  rename(pred.BEP=predicted,conf.lowBEP=conf.low,conf.highBEP=conf.high)
pred.BWP<-pred.BWP %>%
  rename(pred.BWP=predicted,conf.lowBWP=conf.low,conf.highBWP=conf.high)
pred.S<-pred.S %>%
  rename(pred.S=predicted,conf.lowS=conf.low,conf.highS=conf.high)

pred.all<-left_join(pred.BEP,pred.BWP,by="x")
pred.all<-left_join(pred.all,pred.S,by="x")

#mean values in raw data
offspring$Bage<-as.factor(offspring$Bage)

offspringFULL <- offspring %>%
  filter(!is.na(Sage)) %>%
  filter(!is.na(Mage)) %>%
  filter(!is.na(Bage)) %>%
  filter(!is.na(Jincube))

Bage.EP<-offspringFULL %>%
  filter(EPb==1) %>%
  group_by(Bage) %>%
  summarise(meanBageEP=mean(independent))
Bage.EP$Bage<-as.numeric(Bage.EP$Bage)
pred.all<-left_join(pred.all,Bage.EP,by=c("x"="Bage"))
Bage.WP<-offspringFULL %>%
  filter(EPb==0) %>%
  group_by(Bage) %>%
  summarise(meanBageWP=mean(independent))
Bage.WP$Bage<-as.numeric(Bage.WP$Bage)
pred.all<-left_join(pred.all,Bage.WP,by=c("x"="Bage"))
Sage.EP<-offspringFULL %>%
  filter(EPb==1) %>%
  group_by(Sage) %>%
  summarise(meanSageEP=mean(independent))
Sage.EP$Sage<-as.numeric(Sage.EP$Sage)
pred.all<-left_join(pred.all,Sage.EP,by=c("x"="Sage"))

#survival - all: legend
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"), size=2,linetype="dashed")+
  geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(size=2,mapping=aes(x=x,y=pred.BEP,colour="Germ-line"),linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"), size=2,linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  labs( x = "Father Age", y = "Offspring Survival",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.25,0.7))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="right")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                            "Environment","Germ-line & Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                     "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                "Environment","Germ-line & Environment")) +
  


ggsave("inde_all.jpeg", width = 7, height = 4)

#survival - all
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"), size=2,linetype="solid")+
  #geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_point(aes(x=x,y=meanSageEP,colour="Environment"))+
  #geom_line(size=2,mapping=aes(x=x,y=pred.BEP,colour="Germ-line"),linetype="solid")+
  #geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  #geom_point(aes(x=x,y=meanBageEP,colour="Germ-line"))+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"), size=2,linetype="solid")+
  geom_point(aes(x=x,y=meanBageWP,colour="Germ-line & Environment"))+
  #geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  labs( x = "Father Age", y = "Offspring Survival",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.25,0.7))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                          "Environment","Germ-line & Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                                           "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                                           "Environment","Germ-line & Environment")) 
  
ggsave("inde_alln.jpeg", width = 7, height = 6)


#survival EP only
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"), size=2,linetype="dashed")+
  geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(size=2,mapping=aes(x=x,y=pred.BEP,colour="Germ-line"),linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  labs( x = "Father Age", y = "Offspring Survival",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.25,0.7))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Environment" = "blue","Germ-line" = "darkgreen"),breaks=c("Germ-line",
                                                                             "Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Environment" = "blue","Germ-line" = "darkgreen"),breaks=c("Germ-line",
                                                                         "Environment")) 
ggsave("inde2.jpeg", width = 7, height = 6)

#survival EP germ-line only
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(size=2,mapping=aes(x=x,y=pred.BEP,colour="Germ-line"),linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  labs( x = "Father Age", y = "Offspring Survival",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.25,0.7))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen")) 

ggsave("inde1.jpeg", width = 7, height = 6)

#####################################
#weight
summary(weight)

pred.BEP<-ggemmeans(weight,terms=c("BageC"),type="fe", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.BWP<-ggemmeans(weight,terms=c("BageC"),type="fe",condition=c(WPb="1",EPb="0"))
pred.S<-ggemmeans(weight,terms=c("Sage"),type="fe",condition=c(EPb="1",WPb="0"))
pred.S<-filter(pred.S,x<11)
pred.BEP<-pred.BEP %>%
  rename(pred.BEP=predicted,conf.lowBEP=conf.low,conf.highBEP=conf.high)
pred.BWP<-pred.BWP %>%
  rename(pred.BWP=predicted,conf.lowBWP=conf.low,conf.highBWP=conf.high)
pred.S<-pred.S %>%
  rename(pred.S=predicted,conf.lowS=conf.low,conf.highS=conf.high)

pred.all<-left_join(pred.BEP,pred.BWP,by="x")
pred.all<-left_join(pred.all,pred.S,by="x")

#mean values in raw data
offspring$Bage<-as.factor(offspring$Bage)

offspringFULL <- offspring %>%
  filter(!is.na(Sage)) %>%
  filter(!is.na(Mage)) %>%
  filter(!is.na(Bage)) %>%
  filter(!is.na(Jincube))

Bage.EP<-offspringFULL %>%
  filter(EPb==1) %>%
  group_by(Bage) %>%
  summarise(meanBageEP=mean(weight,na.rm=TRUE)/mean(weight.age,na.rm=TRUE))
Bage.EP$Bage<-as.numeric(Bage.EP$Bage)
pred.all<-left_join(pred.all,Bage.EP,by=c("x"="Bage"))
Bage.WP<-offspringFULL %>%
  filter(EPb==0) %>%
  group_by(Bage) %>%
  summarise(meanBageWP=mean(weight,,na.rm=TRUE)/mean(weight.age,na.rm=TRUE))
Bage.WP$Bage<-as.numeric(Bage.WP$Bage)
pred.all<-left_join(pred.all,Bage.WP,by=c("x"="Bage"))
Sage.EP<-offspringFULL %>%
  filter(EPb==1) %>%
  group_by(Sage) %>%
  summarise(meanSageEP=mean(weight,na.rm=TRUE)/mean(weight.age,na.rm=TRUE))
Sage.EP$Sage<-as.numeric(Sage.EP$Sage)
pred.all<-left_join(pred.all,Sage.EP,by=c("x"="Sage"))

#weight - all: lengend
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"), size=2,linetype="dashed")+
  geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(size=2,mapping=aes(x=x,y=pred.BEP,colour="Germ-line"),linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"), size=2,linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  labs( x = "Father Age", y = "Offspring Growth",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.95,1.04),breaks=c(0.96,0.98,1.00,1.02,1.04))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="right")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                          "Environment","Germ-line & Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                           "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                           "Environment","Germ-line & Environment")) 

ggsave("weight_all.jpeg", width = 7, height = 4)

#weight - all
ggplot(pred.all,aes(x=x,y=pred.S))+
  #geom_line(aes(x=x,y=pred.S,colour="Environment"))+
  #geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(mapping=aes(x=x,y=pred.BEP,colour="Germ-line"))+
  #geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  geom_line(aes(x=x,y=pred.BWP,colour="Germ-line & Environment"))+
  #geom_ribbon(aes(ymin=conf.lowBWP,ymax=conf.highBWP,fill="Germ-line & Environment"),alpha=0.3)+
  #geom_point(aes(x=x,y=meanSageEP,colour="Environment"))+
  geom_point(aes(x=x,y=meanBageEP,colour="Germ-line"))+
  geom_point(aes(x=x,y=meanBageWP,colour="Germ-line & Environment"))+
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

ggsave("weight_alln.jpeg", width = 7, height = 6)


#weight - EP only
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(aes(x=x,y=pred.S,colour="Environment"), size=2,linetype="dashed")+
  geom_ribbon(data=pred.all,mapping=aes(ymin=conf.lowS,ymax=conf.highS,fill="Environment"),alpha=0.3)+
  geom_line(size=2,mapping=aes(x=x,y=pred.BEP,colour="Germ-line"),linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  labs( x = "Father Age", y = "Offspring Growth",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.95,1.04),breaks=c(0.96,0.98,1.00,1.02,1.04))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                          "Germ-line & Environment" = "darkred"),breaks=c("Germ-line",
                                                                                          "Environment")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen","Environment" = "blue",
                                           "Germ-line & Environment" = "darkred"),breaks=c("Germ-line","Environment"))
                                                                                           
ggsave("weight2.jpeg", width = 7, height = 6) 

#weight - germ-line only
ggplot(pred.all,aes(x=x,y=pred.S))+
  geom_line(size=2,mapping=aes(x=x,y=pred.BEP,colour="Germ-line"),linetype="dashed")+
  geom_ribbon(aes(ymin=conf.lowBEP,ymax=conf.highBEP,fill="Germ-line"),alpha=0.3)+
  labs( x = "Father Age", y = "Offspring Growth",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.95,1.04),breaks=c(0.96,0.98,1.00,1.02,1.04))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_fill_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen"),breaks=c("Germ-line")) +
  scale_color_manual(name = "Paternal Effects", values = c("Germ-line" = "darkgreen"),breaks=c("Germ-line")) 

ggsave("weight1.jpeg", width = 7, height = 6)

  