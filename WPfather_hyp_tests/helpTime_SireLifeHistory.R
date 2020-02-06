#hypothesis: males that spend more years as a helper have offspring with higher fitness (regardless of age)
#being a high quality male is associated with having high quality parents
#having high quality parents is associated with a territory where males must queue for dominance
#if the good males (which produce genetically good offspring) are spending more time as helpers they are only
#producing WP offspring at older ages


nestRAW <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/nestRAW.csv")


#group study_born by dad_bio
#calc total number chicks, chick mean weight and chick mean independence
#calc total number of WP chicks, weight and independence, and same for EP
male.fit<-study_born %>%
  group_by(dad_bio) %>%
  summarise(total=n(), weight=mean(weight.res,na.rm=TRUE),inde=mean(independent),
        ageMean=mean(Bage,na.rm=TRUE),ageMed=median(Bage,na.rm=TRUE),ageMax=max(Bage,na.rm=TRUE),ageMin=min(Bage,na.rm=TRUE))
male.fitEP<-study_born %>%
  group_by(dad_bio) %>%
  filter(EPb==1) %>%
  summarise(totalEP=n(), weightEP=mean(weight.res,na.rm=TRUE),indeEP=mean(independent),
  ageMeanEP=mean(Bage,na.rm=TRUE),ageMedEP=median(Bage,na.rm=TRUE),ageMaxEP=max(Bage,na.rm=TRUE),ageMinEP=min(Bage,na.rm=TRUE))
male.fitWP<-study_born %>%
  group_by(dad_bio) %>%
  filter(EPb==0) %>%
  summarise(totalWP=n(), weightWP=mean(weight.res,na.rm=TRUE),indeWP=mean(independent),
  ageMeanWP=mean(Bage,na.rm=TRUE),ageMedWP=median(Bage,na.rm=TRUE),ageMaxWP=max(Bage,na.rm=TRUE),ageMinWP=min(Bage,na.rm=TRUE))

male.fit<-left_join(male.fit,male.fitEP)
male.fit<-left_join(male.fit,male.fitWP)

#in nestRAW  group by helper.1 and determine max cohort
table(nestRAW$Age.helper.1)
nestRAW$Age.helper.1<-as.numeric(as.character(nestRAW$Age.helper.1))
nestRAW$Age.helper.2<-as.numeric(as.character(nestRAW$Age.helper.2))
nestRAW$Age.helper.3<-as.numeric(as.character(nestRAW$Age.helper.3))
nestRAW$Age.helper.4<-as.numeric(as.character(nestRAW$Age.helper.4))
nestRAW$Age.helper.5<-as.numeric(as.character(nestRAW$Age.helper.5))

helpers1<-nestRAW %>%
  group_by(Helper.1) %>%
  summarise(last.year1=max(Age.helper.1,na.rm=TRUE))

#repeat for Helper.2 - 5
helpers2<-nestRAW %>%
  group_by(Helper.2) %>%
  summarise(last.year2=max(Age.helper.2,na.rm=TRUE))
helpers3<-nestRAW %>%
  group_by(Helper.3) %>%
  summarise(last.year3=max(Age.helper.3,na.rm=TRUE))
helpers4<-nestRAW %>%
  group_by(Helper.4) %>%
  summarise(last.year4=max(Age.helper.4,na.rm=TRUE))

#join all the helpers together
library(tidyverse)

helpers<-full_join(helpers1,helpers2,by=c("Helper.1"="Helper.2"))
helpers<-full_join(helpers,helpers3,by=c("Helper.1"="Helper.3"))
helpers<-full_join(helpers,helpers4,by=c("Helper.1"="Helper.4"))

#determine the max helper age recorded
helpers<-helpers %>%
  nest(starts_with("last.year"),.key="max.age")%>%
  mutate_at("max.age",map_dbl,max,na.rm=TRUE)
#clean up
helpers<-helpers %>%
  mutate(max.age=ifelse(max.age>9|max.age<1,NA,max.age))

#join helpers to male.fit
male.fit<-left_join(male.fit,helpers,by=c("dad_bio"="Helper.1"))
male.fit<-rename(male.fit,max.help=max.age)
table(male.fit$max.help)
sum(is.na(male.fit$max.help))

#make NAs 0s
male.fit<-male.fit %>% replace_na(list(max.help=0,total=0,totalEP=0,totalWP=0))
#make -inf NA
male.fit<-male.fit %>% na_if('-Inf')

#make max.help categorical
male.fit<-male.fit %>%
  mutate(helped=ifelse(max.help==0,"never","yes")) %>%
  mutate(helped1=ifelse(max.help<2,"less2","2orMore")) %>%
  mutate(helped2=ifelse(max.help<3,"less3","3orMore"))

#####################################################
#weight effect

#overall weight
helpW<-lm(weight~max.help, data=male.fit,weights=total)
summary(helpW)
# no effect overall

#WP weight
helpW.wp<-lm(weightWP~max.help, data=male.fit,weights=totalWP)
summary(helpW.wp)
#no effect on WP weight

#WP weight categorical
summary(helpW.wp1<-lm(weightWP~helped, data=male.fit,weights=totalWP))
#no effect


#WP weight age 2 cut off
summary(helpW.wp1<-lm(weightWP~helped1, data=male.fit,weights=totalWP))
#no effect

#WP weight age 3 cut off
summary(helpW.wp1<-lm(weightWP~helped2, data=male.fit,weights=totalWP))
#no effect

#EP weight
helpW.ep<-lm(weightEP~max.help, data=male.fit,weights=totalEP)
summary(helpW.ep)
#no effect on WP weight

####################
#independence

#overall survival probability
helpI<-glm(inde~max.help, data=male.fit,weights=total,family="binomial")
summary(helpI)
#later helping associated with lower average offspring survival probability
#not quadratic

helpI.wp<-glm(indeWP~max.help, data=male.fit,weights=totalWP,family="binomial")
summary(helpI.wp)
#no effect on WP independence

summary(helpI.wp1<-glm(indeWP~helped2, data=male.fit,weights=totalWP,family="binomial"))

helpI.ep<-glm(indeEP~max.help, data=male.fit,weights=totalEP,family="binomial")
summary(helpI.ep)
#helping for longer associated with lower EP offspring survival 


#################
#siring success
mean(male.fit$total,na.rm=TRUE)
var(male.fit$total,na.rm=TRUE)

#overall
summary(helpLRS<-glmmTMB(total/(max.help+1)~max.help, data=male.fit,weights=total,family="nbinom2"))
#helping for longer improves your overall LRS
#probably because helping for longer means you live longer
#per year of helping it has a strongly negative effect

#WP
summary(helpLRS.wp<-glmmTMB(totalWP~max.help, data=male.fit,weights=total,family="nbinom2"))
#it has a neg effect on WP success overall


######################################################
#siring date effects on weight

#WP
#mean WP siring date
summary(help.lhWP<-lm(weightWP~ageMeanWP, data=male.fit,weights=totalWP))
#no effect  
  
#max WP siring date
summary(help.lhWP<-lm(weightWP~ageMaxWP, data=male.fit,weights=totalWP))
#no effect  

#all offspring
#mean siring date
summary(help.lhEP<-lm(weight~ageMean, data=male.fit,weights=total))
#no effect  

#max siring date
summary(help.lh<-lm(weight~ageMax, data=male.fit,weights=total))
#no effect  

######################################################
#siring date effects on independence

#overall
#mean age
summary(helpI<-glm(inde~ageMean, data=male.fit,weights=total,family="binomial"))
#no effect

#overall
#max age
summary(helpI<-glm(inde~ageMax, data=male.fit,weights=total,family="binomial"))
#no effect


#WP mean age
summary(helpI<-glm(indeWP~ageMeanWP, data=male.fit,weights=totalWP,family="binomial"))
#no effect

#WP max age
summary(helpI<-glm(indeWP~ageMaxWP, data=male.fit,weights=totalWP,family="binomial"))
#no effect

# mean age overall on WPinde
summary(helpI<-glm(indeWP~ageMean, data=male.fit,weights=totalWP,family="binomial"))
#no effect

#WP max age overall on WPinde
summary(helpI<-glm(indeWP~ageMax, data=male.fit,weights=totalWP,family="binomial"))
#no effect
