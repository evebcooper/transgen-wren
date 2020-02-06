###########
#graphing inde
#use predictions from mixed model
#calculated CIs from a REs model 

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")

#put 9 and 10 year olds at 8 
offspring<-offspring %>%
  mutate(BageC=ifelse(Bage>8,8,Bage)) %>%
  mutate(SageC=ifelse(Sage>8,8,Sage)) %>%
  mutate(MageC=ifelse(Mage>8,8,Mage))

#weight graphs
library(ggplot2)
library(dplyr)
library(ggeffects)
library(prediction)

##############################################################################################################
#survival to inde

#re-run inde model with and without random effects
#inde RE
surv<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
              EPb+Jincube+weight.res+
              (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
            data=offspring, family="binomial")
summary(surv)
#ES: 0.12 and p = 0.015 (se 0.047)
#EPB es = -0.055. se =0.04

#inde FE
survFE<-glm(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
              EPb+Jincube+weight.res,data=offspring, family="binomial")
summary(survFE)
#ES 0.050 and p=0.12 (se = 0.032)
#EPB es = -0.039. se =0.029

#dataframe
indeDat<-offspring %>%
  select(Mage,MageC,Bage,Sage,BageC,SageC, lifespanB,lifespanS,lifespanB,lifespanM,Jincube,weight.res,EPb,
         cohort, mum, dad_bio,dad_soc,WPb,independent,sex)
indeDat<-na.omit(indeDat)

#summarise age raw data 
indeDatWPB <- indeDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(independent))
indeDatEPS <- indeDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(num=n(),surv=mean(independent))
indeDatEPB <- indeDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(independent))
indeDatmum <- indeDat%>%
  group_by(MageC)%>%
  summarise(num=n(),surv=mean(independent))

############################################################################################################################

#######################################################################################################################
#####################
#WP Bage estimates
#pull predictions from the RE model (surv)
newdatWPB<-new_data(surv,terms=c("Bage[1:10]","cohort[sample=5]","mum[sample=40]","dad_bio[sample=40]","dad_soc[sample=40]"),
                    condition=c(EPb=0,WPb=1))
WpredWPB<-predict(surv,newdata=newdatWPB,allow.new.levels=TRUE,type="response")
#start:11:56am - 12
newdatWPB$fit <- cbind(WpredWPB)
newdatWPB<-filter(newdatWPB,Bage<9)
#newdatWPB<-mutate(newdatWPB,BageC=ifelse(Bage>9,9,Bage))

sum.datWPB<- newdatWPB %>%
  group_by(Bage) %>%
  summarise(fit=mean(fit))
sum.datWPB$raw.surv <- cbind(indeDatWPB$surv)
sum.datWPB$sample <- cbind(indeDatWPB$num) 

remove("newdatWPB")
remove("WpredWPB")

#pull SE from the FE model 
WPBdatFE<-new_data(survFE,terms=c("Bage[1:10]"),condition=c(EPB=0,WPb=1))
WseWPB<-predict(survFE,newdata=WPBdatFE,se.fit=TRUE,type="response")

sum.datWPB$se<-cbind(WseWPB$se.fit[1:8])
sum.datWPB<-sum.datWPB %>%
  mutate(lci=fit-(1.96*se))%>%
  mutate(uci=fit+(1.96*se))%>%
  mutate(dad.type="WPB")


ggplot(data=sum.datWPB,aes(x=Bage,y=fit))+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_point(aes(x=Bage,y=raw.surv))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci),alpha=0.1)+
  theme_bw()

#######################################################################################################################
#####################
#EP Bage estimates
#pull predictions from the RE model (surv)
newdatEPB<-new_data(surv,terms=c("Bage[1:10]","cohort[sample=15]","mum[sample=60]","dad_bio[sample=70]","dad_soc[0]"),
                    condition=c(EPb=1,WPb=0))
WpredEPB<-predict(surv,newdata=newdatEPB,allow.new.levels=TRUE,type="response")
#start:12:08 -12:12
newdatEPB$fit <- cbind(WpredEPB)
newdatEPB<-filter(newdatEPB,Bage<9)

#newdatEPB<-mutate(newdatEPB,BageC=ifelse(Bage>9,9,Bage))

sum.datEPB<- newdatEPB %>%
  group_by(Bage) %>%
  summarise(fit=mean(fit))


sum.datEPB$raw.surv <- cbind(indeDatEPB$surv)
sum.datEPB$sample <- cbind(indeDatEPB$num) 


remove("newdatEPB")
remove("WpredEPB")

#pull SE from the FE model 
EPBdatFE<-new_data(survFE,terms=c("Bage[1:10]"),condition=c(EPB=1,WPb=0))
WseEPB<-predict(survFE,newdata=EPBdatFE,se.fit=TRUE,type="response")

sum.datEPB$se<-cbind(WseEPB$se.fit[1:8])
sum.datEPB<-sum.datEPB %>%
  mutate(lci=fit-(1.96*se))%>%
  mutate(uci=fit+(1.96*se))%>%
  mutate(dad.type="EPB")


ggplot(data=sum.datEPB,aes(x=Bage,y=fit))+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_point(aes(x=Bage,y=raw.surv))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci),alpha=0.1)+
  theme_bw()
#the problem here is that the CIs predicted by the FE model are smaller than those predicted by RE model
#######################################################################################################################
#####################
#EP Sage estimates
#pull predictions from the RE model (surv)
newdatEPS<-new_data(surv,terms=c("Sage[1:10]","cohort[sample=15]","mum[0]","dad_bio[0]","dad_soc[sample=100]"),
                    condition=c(EPb=1,WPb=0))
WpredEPS<-predict(surv,newdata=newdatEPS,allow.new.levels=TRUE,type="response")
newdatEPS$fit <- cbind(WpredEPS)
#newdatEPS<-mutate(newdatEPS,BageC=ifelse(Bage>9,9,Bage))
newdatEPS<-filter(newdatEPS,Sage<9)

sum.datEPS<- newdatEPS %>%
  group_by(Sage) %>%
  summarise(fit=mean(fit)) %>%
  rename(Bage=Sage)


sum.datEPS$raw.surv <- cbind(indeDatEPS$surv)
sum.datEPS$sample <- cbind(indeDatEPS$num) 

remove("newdatEPS")
remove("WpredEPS")

#pull SE from the FE model 
EPSdatFE<-new_data(survFE,terms=c("Sage[1:10]"),condition=c(EPb=1,WPb=0))
WseEPS<-predict(survFE,newdata=EPSdatFE,se.fit=TRUE,type="response")

sum.datEPS$se<-cbind(WseEPS$se.fit[1:8])
sum.datEPS<-sum.datEPS %>%
  mutate(lci=fit-(1.96*se))%>%
  mutate(uci=fit+(1.96*se))%>%
  mutate(dad.type="EPS")


ggplot(data=sum.datEPS,aes(x=Bage,y=fit))+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_point(aes(x=Bage,y=raw.surv))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci),alpha=0.1)+
  theme_bw()

###############################################
#mom age

#pull predictions from the RE model (surv)
newdatmum<-new_data(surv,terms=c("Mage[1:10]","cohort[sample=15]","mum[sample=100]","dad_bio[0]","dad_soc[0]"))
Wpredmum<-predict(surv,newdata=newdatmum,allow.new.levels=TRUE,type="response")
newdatmum$fit <- cbind(Wpredmum)
#newdatEPS<-mutate(newdatEPS,BageC=ifelse(Bage>9,9,Bage))
newdatmum<-filter(newdatmum,Mage<9)

sum.datmum<- newdatmum %>%
  group_by(Mage) %>%
  summarise(fit=mean(fit)) %>%
  rename(Bage=Mage)

sum.datmum$raw.surv <- cbind(indeDatmum$surv)
sum.datmum$sample <- cbind(indeDatmum$num) 

remove("newdatmum")
remove("Wpredmum")

#pull SE from the FE model 
mumdatFE<-new_data(survFE,terms=c("Mage[1:10]"),condition=c(EPb=1,WPb=0))
Wsemum<-predict(survFE,newdata=mumdatFE,se.fit=TRUE,type="response")

sum.datmum$se<-cbind(Wsemum$se.fit[1:8])
sum.datmum<-sum.datmum %>%
  mutate(lci=fit-(1.96*se))%>%
  mutate(uci=fit+(1.96*se))%>%
  mutate(dad.type="mum")


ggplot(data=sum.datmum,aes(x=Bage,y=fit))+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_point(aes(x=Bage,y=raw.surv))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci),alpha=0.1)+
  theme_bw()



##################################################################################################
#graph altogether

#merge 3 datasets to one
inde.sumDat<-rbind(sum.datEPB,sum.datWPB)
inde.sumDat<-rbind(inde.sumDat,sum.datEPS)
inde.sumDat<-rbind(inde.sumDat,sum.datmum)

ggplot(data=inde.sumDat,aes(x=Bage,y=fit,colour=dad.type))+
  geom_point(data=subset(inde.sumDat,dad.type=="WPB"),mapping=aes(x=Bage,y=raw.surv,size=sample))+
  geom_point(data=subset(inde.sumDat,dad.type!="WPB"),mapping=aes(x=Bage,y=raw.surv,size=sample))+
  geom_line(mapping=aes(x=Bage,y=fit),size=2)+
  geom_line(data=subset(inde.sumDat,dad.type=="WPB"),mapping=aes(x=Bage,y=fit),size=2)+
  #geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci,fill=dad.type),color=NA,alpha=0.1)+
  #geom_ribbon(data=subset(inde.sumDat,dad.type=="WPB"),mapping=aes(x=Bage,ymax=uci,ymin=lci,fill=dad.type),color=NA,alpha=0.5)+
  labs(x="Parent Age",y="Chick Survival")+
  theme_bw()


