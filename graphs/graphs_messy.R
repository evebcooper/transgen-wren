library(modelr)
library(ggplot2)
library(ggeffects)

#weight model
weight<-lmer(weight~Mage+Bage:EPb+Bage:WPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                lifespanM+EPb+Jincube+weight.age+pre1992+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=offspring)
summary(weight)

newdat<-new_data(weight,terms=c("Bage"),condition=c(EPb=0))
pred<-predict(weight,newdata=newdat,re.form=NA)
bb<-(bootMer(weight,function(x)predict(x,re.form=NA),nsim=10000))

weightDat$lci <- apply(bb$t, 2, quantile, 0.025)
weightDat$uci <- apply(bb$t, 2, quantile, 0.975)
weightDat$weightpred <- predict(weight, re.form=NA)





#weight FE only
weightFE<-lm(weight~Mage+Bage:EPb+Bage:WPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992,
             data=offspring)
summary(weightFE)

#weight
#re-run model without the effect of bio dad age
weightG<-lmer(weight~Mage+Bage:EPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring)
summary(weightG)

weightDat<-offspring %>%
  select(Mage,Bage,Sage,lifespanB,lifespanS,lifespanB,lifespanM,Jincube,weight.age,pre1992,weight,Bage,EPb,
         cohort, mum, dad_bio,dad_soc,WPb)
weightDat<-na.omit(weightDat)

weightDat<-weightDat %>%
  mutate(resid=resid(weightG)) %>%
  mutate(weightCor=weight+resid)

#mean values in raw data
weightDat$Bage<-as.numeric(weightDat$Bage)

weightDat1 <- weightDat %>%
  mutate(BageC=ifelse(Bage==10,8,ifelse(Bage==9,8,Bage)))%>%
  #filter(Jincube<0.43)%>%
  filter(EPb==0) %>%
  group_by(BageC)%>%
  summarise(lci=mean(lci),uci=mean(uci),pred=mean(weightpred),weight=mean(weightCor))%>%
  mutate(ci=(uci-lci)/2)



#model predictions  
pred.BEP<-ggemmeans(weight,terms=c("Bage"),type='fe',condition=c(EPb="1",WPb="0")) 
pred.BWP<-ggemmeans(weight,terms=c("Bage"),type="fe",condition=c(WPb="1")) 
pred.S<-ggpredict(weight,terms=c("Sage"),type="sim",condition=c(EPb="1",WPb="0"))
pred.S<-filter(pred.S,x<9)
pred.BWP<-filter(pred.BWP,x<9)
pred.BEP<-filter(pred.BEP,x<9)
pred.BEP<-pred.BEP %>%
  rename(pred.BEP=predicted)
pred.BWP<-pred.BWP %>%
  rename(pred.BWP=predicted,conf.lowBWP=conf.low,conf.highBWP=conf.high)
#conf.lowBWP=conf.low,conf.highBWP=conf.high
pred.S<-pred.S %>%
  rename(pred.S=predicted)

pred.all<-left_join(pred.BEP,pred.BWP,by="x")
pred.all<-left_join(pred.all,pred.S,by="x")
pred.all<-left_join(pred.all,weightDat1,by=c("x"="BageC"))

ggplot(data=pred.all,aes(x=x,y=pred.BWP))+
  geom_line(color="red")+
  geom_line(aes(x=x,y=pred.BEP),colour="green")+
  geom_line(aes(x=x,y=pred.S),colour="blue")+
  geom_point(aes(x=x,y=weight),colour="red")
  #geom_smooth(data=weightDat1,mapping=aes(x=BageC,y=pred.BEP),method="lm",se=FALSE,color="red")+
  geom_ribbon(mapping=aes(x=x,ymax=conf.highBWP,ymin=conf.lowBWP),alpha=0.1)+
  #geom_smooth(data=weightDat1,mapping=aes(x=BageC,y=lci),linetype="dashed",color="black",method="lm",se=FALSE)+
  #geom_smooth(data=weightDat1,mapping=aes(x=BageC,y=uci),linetype="dashed",color="black",method="lm",se=FALSE)+
  theme_classic()
  
  
#plot
ggplot(data=pred.all,aes(x=x,y=weight))+
  geom_point(aes(x=x,y=weight,colour="WP"))+
  #geom_errorbar(data=weightDat1,mapping=aes(x=BageC,ymax=weight+sd,ymin=weight-sd))
 geom_line(aes(x=x,y=pred.BWP,colour="WP"), size=1)+
  geom_line(aes(x=x,y=pred.S,colour="EP S"))+
  geom_line(aes(x=x,y=pred.BEP,colour="EP B"))+
  scale_fill_manual(name = "Paternal Effects", values = c("EP B" = "darkgreen","EP S" = "blue",
                                                          "WP" = "darkred"),breaks=c("EP B" = "darkgreen","EP S" = "blue",
                                                                                     "WP" = "darkred")) +
  scale_colour_manual(name = "Paternal Effects", values = c("EP B" = "darkgreen","EP S" = "blue",
                                                          "WP" = "darkred"),breaks=c("EP B" = "darkgreen","EP S" = "blue",
                                                                                     "WP" = "darkred"))


ggplot(pred.all,aes(x=x,y=pred.S))+
  abline(aes(x=x,y=pred.S,colour="Environment"), size=2,linetype="dashed")+
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
                                                                                                           "Environment","Germ-line & Environment")) 
  