library(ggeffects)
library(modelr)
library(ggplot2)
library(dplyr)

#shorten everyones age to 9
offspring_indg<-offspring %>%
  mutate(BageC=ifelse(Bage>9,9,Bage)) %>%
  mutate(SageC=ifelse(Sage>9,9,Sage)) %>%
  mutate(MageC=ifelse(Mage>9,9,Bage))
  
#survival to inde
inde.sd<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.sd)



####################
#WP Bage
#remove WP Bage 
inde.WPB<-glmer(independent~Mage+Bage:EPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.WPB)

#dataframe
survDat<-offspring_indg %>%
  select(Mage,BageC,Sage,lifespanB,lifespanS,lifespanB,lifespanM,Jincube,EPb,
         cohort, mum, dad_bio,dad_soc,WPb,independent)
survDat<-na.omit(survDat)

survDat$independent-predict(inde.sd,type="response")

#calc residuals and add to raw data
survDat<-survDat %>%
  mutate(resid=(independent-predict(inde.sd,type="response"))) %>%
  mutate(indCor=(independent)-(resid))
#summarise age raw data to one row per age
survDat1 <- survDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(indCor),rawsurv=mean(independent))
#WP father age

#response type
newdat1<-new_data(inde.sd,terms=c("Bage"),condition=c(EPb=0,WPb=1))
bb1<-(bootMer(inde.sd,function(x)predict(x,re.form=NA,newdata=newdat1,type="link"),nsim=5))

#response type
newdat1<-new_data(inde.sd,terms=c("Bage","cohort[sample=10]","dad_bio [sample=20]","dad_soc [sample=20]","mum [sample=20]"),condition=c(EPb=0,WPb=1))
bb1<-(bootMer(inde.sd,function(x)predict(x,re.form=NULL,newdata=newdat1,type="response"),nsim=1000))
#add predictions  
newdat1$lci.re <- apply(bb1$t, 2, quantile, 0.025)
newdat1$uci.re <- apply(bb1$t, 2, quantile, 0.975)
newdat1$indpred.re <- apply(bb1$t, 2, quantile, 0.5)

ind.pred.sum1<-newdat1 %>%
  filter(EPb==0) %>%
  group_by(Bage)%>%
  summarise(lci=mean(lci.re),uci=mean(uci.re),pred=mean(indpred.re))
####################################################################################################################

####################
#EP Bage
#remove EP Bage 
inde.EPB<-glmer(independent~Mage+Bage:WPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.EPB)

#dataframe
survDatEPB<-offspring_indg %>%
  select(Mage,BageC,Sage,lifespanB,lifespanS,lifespanB,lifespanM,Jincube,EPb,
         cohort, mum, dad_bio,dad_soc,WPb,independent)
survDatEPB<-na.omit(survDatEPB)
#calc residuals and add to raw data
survDatEPB<-survDatEPB %>%
  mutate(resid=plogis(resid(inde.EPB))) %>%
  mutate(indCor=(independent)-(resid))
#summarise age raw data to one row per age
survDatEPB1 <- survDatEPB%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(indCor),rawsurv=mean(independent))

#generate model predictions
newdatEPB<-new_data(inde.sd,terms=c("Bage","cohort[sample=10]","dad_bio [sample=20]","dad_soc [sample=20]","mum [sample=20]"),condition=c(EPb=1,WPb=0))
bbEPB<-(bootMer(inde.sd,function(x)predict(x,re.form=NULL,newdata=newdatEPB,type="response"),nsim=500))
#add predictions  
newdatEPB$lci <- apply(bbEPB$t, 2, quantile, 0.025)
newdatEPB$uci <- apply(bbEPB$t, 2, quantile, 0.975)
newdatEPB$indpred <- apply(bbEPB$t, 2, quantile, 0.5)

ind.pred.sumEPB<-newdatEPB %>%
  filter(EPb==1) %>%
  group_by(Bage)%>%
  summarise(lci=mean(lci),uci=mean(uci),pred=mean(indpred))

####################################################################################################################

####################
#EP Sage
#remove EP Sage 
inde.EPS<-glmer(independent~Mage+Bage:WPb+Bage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.EPS)

#dataframe
survDatEPS<-offspring_indg %>%
  select(Mage,Bage,SageC,lifespanB,lifespanS,lifespanB,lifespanM,Jincube,EPb,
         cohort, mum, dad_bio,dad_soc,WPb,independent)
survDatEPS<-na.omit(survDatEPS)
#calc residuals and add to raw data
survDatEPS<-survDatEPS %>%
  mutate(resid=plogis(resid(inde.EPS))) %>%
  mutate(indCor=(independent)-(resid))
#summarise age raw data to one row per age
survDatEPS1 <- survDatEPS%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(num=n(),surv=mean(indCor),rawsurv=mean(independent))

#generate model predictions
newdatEPS<-new_data(inde.sd,terms=c("Sage","cohort[sample=10]","dad_bio [sample=20]","dad_soc [sample=20]","mum [sample=20]"),condition=c(EPb=1,WPb=0))
bbEPS<-(bootMer(inde.sd,function(x)predict(x,re.form=NULL,newdata=newdatEPS,type="response"),nsim=500))
#add predictions  
newdatEPS$lci <- apply(bbEPS$t, 2, quantile, 0.025)
newdatEPS$uci <- apply(bbEPS$t, 2, quantile, 0.975)
newdatEPS$indpred <- apply(bbEPS$t, 2, quantile, 0.5)

ind.pred.sumEPS<-newdatEPS %>%
  filter(EPb==1) %>%
  group_by(Sage)%>%
  summarise(lci=mean(lci),uci=mean(uci),pred=mean(indpred))

#######################################################################################################################
ggplot(data=ind.pred.sum1,aes(x=Bage,y=pred))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci,fill="WP"),alpha=0.1)+
  geom_line(aes(x=Bage,y=pred,color="WP"))+
  #geom_point(data= survDat1,aes(x=BageC,y=surv))+
  geom_point(data= survDat1,aes(x=BageC,y=rawsurv,colour="WP"))+
  geom_ribbon(data=ind.pred.sumEPB,mapping=aes(x=Bage,ymax=uci,ymin=lci,fill="EP genetic"),alpha=0.1)+
  geom_line(data=ind.pred.sumEPB,mapping=aes(x=Bage,y=pred,colour="EP genetic"))+
  geom_point(data= survDatEPB1,aes(x=BageC,y=rawsurv,colour="EP genetic"))+
  geom_ribbon(data=ind.pred.sumEPS,mapping=aes(x=Sage,ymax=uci,ymin=lci,fill="EP social"),alpha=0.1)+
  geom_line(data=ind.pred.sumEPS,mapping=aes(x=Sage,y=pred,colour="EP social"))+
  geom_point(data= survDatEPS1,aes(x=SageC,y=rawsurv,colour="EP social"))+
  scale_x_continuous(limits=c(1,9))+
  #scale_y_continuous(limits=c(6.75,10))+
  scale_fill_manual(name = "", values = c("WP" = "darkred","EP genetic" = "green", "EP social" = "darkblue")) +
  scale_color_manual(name = "", values = c("WP" = "darkred","EP genetic" = "green", "EP social" = "darkblue")) +
  labs(title="Within-Pair Father Age",x="Age",y="Survival to independence")+
  theme_classic()





  
  


