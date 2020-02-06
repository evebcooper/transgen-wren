###########
#graphing weight
#use predictions from mixed model
#calculated CIs from a REs model 

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")

#weight graphs
library(ggplot2)
library(dplyr)
library(ggeffects)
library(prediction)

#put 9 and 10 year olds at 8 
offspring<-offspring %>%
  mutate(BageC=ifelse(Bage>8,8,Bage)) %>%
  mutate(SageC=ifelse(Sage>8,8,Sage))
#######################################################################################
#weight
#weight w/ RE
wGraph<-lmer(weight~Mage+BageC:EPb+BageC:WPb+SageC:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspring)
summary(wGraph)
#WP Bage 0.03, 0.014
#EP Bage -0.00, 0.012
#Sage -0.00, 0.012

#weight w/ RE: all Bages included
offspring$Bage<-as.numeric(offspring$Bage)
wGraph1<-lmer(weight~Mage+Bage:EPb+Bage:WPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspring)
summary(wGraph1)

#weight w/FE
weightFE<-lm(weight~Mage+BageC:EPb+BageC:WPb+SageC:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992,
             data=offspring)
summary(weightFE)
#WP Bage 0.02, 0.012
#EP Bage 0.00, 0.011
#Sage -0.00, 0.011

weightFE1<-lm(weight~Mage+Bage:EPb+Bage:WPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992,
             data=offspring)
summary(weightFE1)
#WP Bage 0.02, 0.012

#dataframe
weightDat<-offspring %>%
  select(Mage,BageC,Bage,Sage,SageC, lifespanB,lifespanS,lifespanB,lifespanM,Jincube,weight.age,pre1992,weight,EPb,
         cohort, mum, dad_bio,dad_soc,WPb)
weightDat<-na.omit(weightDat)


#summarise age raw data to 9 rows
weightDatWPB <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),weight=mean(weight))
weightDatEPS <- weightDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(num=n(),weight=mean(weight))
weightDatEPB <- weightDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(num=n(),weight=mean(weight))

############################################################################################################################

#######################################################################################################################
#####################
#WP Bage estimates
#pull predictions from the RE model (wGraph1)
newdatWPB<-new_data(wGraph1,terms=c("Bage[1:10]","cohort[sample=10]","mum[sample=45]","dad_bio[sample=45]","dad_soc[sample=45]"),
                    condition=c(EPb=0,WPb=1))
WpredWPB<-predict(wGraph1,newdata=newdatWPB,allow.new.levels=TRUE)

newdatWPB$fit <- cbind(WpredWPB)
newdatWPB<-filter(newdatWPB,Bage<9)

sum.datWPB<- newdatWPB %>%
  group_by(Bage) %>%
  summarise(fit=mean(fit))
sum.datWPB$raw.weight <- cbind(weightDatWPB$weight)
sum.datWPB$sample <- cbind(weightDatWPB$num) 

remove("newdatWPB")
remove("WpredWPB")

#pull SE from the FE model (weightFE1)
WPBdatFE<-new_data(weightFE1,terms=c("Bage[1:8]"),condition=c(EPB=0,WPb=1))
WseWPB<-predict(weightFE1,newdata=WPBdatFE,se.fit=TRUE)

sum.datWPB$se<-cbind(WseWPB$se.fit[1:8])
sum.datWPB<-sum.datWPB %>%
  mutate(lci=fit-(1.96*se))%>%
  mutate(uci=fit+(1.96*se))%>%
  mutate(dad.type="WPB")


ggplot(data=sum.datWPB,aes(x=Bage,y=fit))+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_point(aes(x=Bage,y=raw.weight))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci),alpha=0.1)+
  theme_bw()



#####################
#EP Bage estimates
#pull predictions from the RE model (wGraph)
newdatEPB<-new_data(wGraph1,terms=c("Bage[1:10]","cohort[10]","mum[sample=40]","dad_bio[sample=40]","dad_soc[sample=40]"),
                 condition=c(EPb=1,WPb=0))
WpredEPB<-predict(wGraph1,newdata=newdatEPB,allow.new.levels=TRUE)
newdatEPB$fit <- cbind(WpredEPB)
newdatEPB<-filter(newdatEPB,Bage<9)


sum.datEPB<- newdatEPB %>%
  group_by(Bage) %>%
  summarise(fit=mean(fit))
sum.datEPB$raw.weight <- cbind(weightDatEPB$weight)
sum.datEPB$sample <- cbind(weightDatEPB$num) 

remove("newdatEPB")
remove("WpredEPB")

#pull SE from the FE model (weightFE1)
EPBdatFE<-new_data(weightFE1,terms=c("Bage[1:8]"),condition=c(EPb=1,WPb=0))
WseEPB<-predict(weightFE1,newdata=EPBdatFE,se.fit=TRUE)

sum.datEPB$se<-cbind(WseEPB$se.fit)
sum.datEPB<-sum.datEPB %>%
  mutate(lci=fit-(1.96*se))%>%
  mutate(uci=fit+(1.96*se)) %>%
  mutate(dad.type="EPB")

  
ggplot(data=sum.datEPB,aes(x=Bage,y=fit))+
  geom_point(aes(x=Bage,y=raw.weight))+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci),alpha=0.1)+
  theme_bw()

#####################
#EP Sage estimates
#pull predictions from the RE model (wGraph)
newdatEPS<-new_data(wGraph1,terms=c("Sage[1:10]",
                            "cohort[sample=10]","mum[sample=40]","dad_bio[sample=40]","dad_soc[sample=40]"),
                    condition=c(EPb=1,WPb=0))
WpredEPS<-predict(wGraph1,newdata=newdatEPS,allow.new.levels=TRUE)
newdatEPS$fit <- cbind(WpredEPS)
newdatEPS<-filter(newdatEPS,Sage<9)

sum.datEPS<- newdatEPS %>%
  group_by(Sage) %>%
  summarise(fit=mean(fit))
sum.datEPS$raw.weight <- cbind(weightDatEPS$weight)
sum.datEPS$sample <- cbind(weightDatEPS$num) 

remove("newdatEPS")
remove("WpredEPS")

#pull SE from the FE model (weightFE)
EPSdatFE<-new_data(weightFE1,terms=c("Sage[1:8]"),condition=c(EPb=1,WPb=0))
WseEPS<-predict(weightFE1,newdata=EPSdatFE,se.fit=TRUE)

sum.datEPS$se<-cbind(WseEPS$se.fit)
sum.datEPS<-sum.datEPS %>%
  mutate(lci=fit-(1.96*se))%>%
  mutate(uci=fit+(1.96*se))%>%
  mutate(dad.type="EPS")%>%
  rename(Bage=Sage)



ggplot(data=sum.datEPS,aes(x=Bage,y=fit))+
  geom_point(aes(x=Bage,y=raw.weight))+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci),alpha=0.1)+
  theme_bw()

###############################################################
#graph altogether

#merge 3 datasets to one
weight.sumDat<-rbind(sum.datEPB,sum.datWPB)
weight.sumDat<-rbind(weight.sumDat,sum.datEPS)


ggplot(data=weight.sumDat,aes(x=Bage,y=fit,colour=dad.type))+
  geom_point(data=subset(weight.sumDat,dad.type=="WPB"),mapping=aes(x=Bage,y=raw.weight),size=1.5,shape=19)+
  geom_point(data=subset(weight.sumDat,dad.type=="EPB"),mapping=aes(x=Bage,y=raw.weight),size=1.5,shape=15)+
  geom_point(data=subset(weight.sumDat,dad.type=="EPS"),mapping=aes(x=Bage,y=raw.weight),size=1.5,shape=17)+
  geom_line(mapping=aes(x=Bage,y=fit))+
  geom_line(data=subset(weight.sumDat,dad.type=="WPB"),mapping=aes(x=Bage,y=fit),size=1)+
  #geom_ribbon(mapping=aes(x=Bage,ymax=uci,ymin=lci,fill=dad.type),color=NA,alpha=0.1)+
  #geom_ribbon(data=subset(weight.sumDat,dad.type=="WPB"),mapping=aes(x=Bage,ymax=uci,ymin=lci,fill=dad.type),color=NA,alpha=0.5)+
  labs(x="Male Age",y="Chick Weight")+
  #coord_cartesian(ylim=c(6.85,7.15))+
  theme_bw()




##########################################################################################################
#TRASHED CODE


#########
#correcting raw data by residuals
#I've removed this step as it does not change the data means!
#WP Bage removed
WPBres.m<-lmer(weight~Mage+Bage:EPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                 lifespanM+EPb+Jincube+weight.age+pre1992+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=offspring)
#EP Sage removed
EPSres.m<-lmer(weight~Mage+Bage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                 lifespanM+EPb+Jincube+weight.age+pre1992+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=offspring)
#EP Bage removed
EPBres.m<-lmer(weight~Mage+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                 lifespanM+EPb+Jincube+weight.age+pre1992+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=offspring)