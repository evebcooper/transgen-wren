###########
#graphing weight/inde
#use predictions from mixed model
#calculated CIs from a REs model 

#weight graphs
library(modelr)
library(ggplot2)
library(ggeffects)

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
#WP Bage 0.03, 0.014
#EP Bage -0.00, 0.012
#Sage -0.00, 0.012

#weight w/FE
weightFE<-lm(weight~Mage+BageC:EPb+BageC:WPb+SageC:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992,
             data=offspring)
summary(weightFE)
#WP Bage 0.02, 0.012
#EP Bage 0.00, 0.011
#Sage -0.00, 0.011


#dataframe
weightDat<-offspring %>%
  select(Mage,BageC,Bage,Sage,SageC, lifespanB,lifespanS,lifespanB,lifespanM,Jincube,weight.age,pre1992,weight,EPb,
         cohort, mum, dad_bio,dad_soc,WPb)
weightDat<-na.omit(weightDat)

###################
#WP father 

#correcting raw data by residuals
#re-run model without the effect of WP dad age
#BageC rounded
WPBres.m<-lmer(weight~Mage+BageC:EPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                lifespanM+EPb+Jincube+weight.age+pre1992+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=offspring)
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(WPBres=resid(WPBres.m)) %>%
  mutate(WPBcor=weight-WPBres)
#summarise age raw data to 9 rows
weightDat1 <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),weight=mean(WPBcor))



#pull predictions from the RE model (wGraph)
newdat<-new_data(wGraph,terms=c("BageC","cohort","mum[sample=50]","dad_bio[sample=50]","dad_soc[sample=50]"),
                 condition=c(EPb=0,WPb=1))
Wpred<-predict(wGraph,newdata=newdat,re.form~NULL,allow.new.levels=TRUE)


newdat$fit <- cbind(Wpred)

sum.dat<- newdat %>%
  group_by(BageC) %>%
  summarise(fitM=mean(fit))
sum.dat$raw.weight <- cbind(weightDat1$weight)
sum.dat$sample <- cbind(weightDat1$num) 

#pull SE from the FE model (weightFE)
datFE<-new_data(weightFE,terms=c("BageC"),condition=c(EPb=0,WPb=1))
Wse<-predict(weightFE,newdata=newdat,se.fit=TRUE)

datFE$SE<-cbind(Wse$se.fit)

ggplot(data=sum.dat,aes(x=BageC,y=fitM))+
  geom_point(aes(x=BageC,y=raw.weight))+
  geom_line(mapping=aes(x=BageC,y=fitM))+
  #geom_ribbon(mapping=aes(x=BageC,ymax=uciM,ymin=lciM),alpha=0.1)+
  theme_classic()

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

#inde FE
survFE<-glm(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
              EPb+Jincube+weight.res,data=offspring, family="binomial")
summary(survFE)
#ES 0.050 and p=0.12 (se = 0.032)


#just removing dad_bio and cohort since these have the lowest variances
survFE1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
              EPb+Jincube+weight.res+
              +(1|mum)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
            data=offspring, family="binomial")
summary(survFE1)
#ES of 0.102 and p = 0.025

#just keeping dad soc
survFE2<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+weight.res+
                 +(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(survFE2)
# ES of 0.09 and p=0.019

#just keeping dad bio
survFE3<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+weight.res+
                 +(1|dad_bio),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(survFE3)
# ES of 0.08 and p =0.032 (std. error 0.037)


