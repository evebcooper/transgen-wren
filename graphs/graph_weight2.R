#weight graphs
library(modelr)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(merTools)
library(gpuR)


offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")

#put 9 and 10 year olds at 8 
offspring<-offspring %>%
  mutate(BageC=ifelse(Bage>8,8,Bage)) %>%
  mutate(SageC=ifelse(Sage>8,8,Sage)) %>%
  mutate(MageC=ifelse(Mage>8,8,Mage))

#base model
weight<-lmer(weight~Mage+Bage:EPb+Bage:WPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspring)
summary(weight)


#dataframe
weightDat<-offspring %>%
  dplyr::select(Mage,MageC,BageC,Bage,Sage,SageC,lifespanB,
         lifespanS,lifespanB,lifespanM,Jincube,weight.age,pre1992,weight,EPb,
         cohort, mum, dad_bio,dad_soc,WPb)
weightDat<-na.omit(weightDat)


###################
#WP father 
#re-run model without the effect of WP dad age
weightG<-lmer(weight~Mage+Bage:EPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                lifespanM+EPb+Jincube+weight.age+pre1992+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=offspring)
summary(weightG)
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(resid=resid(weightG)) %>%
  mutate(weightCor=weight+resid)
#summarise age raw data to 8 rows
weightDat1 <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),weight=mean(weight))

ggplot(data=weightDat1,aes(x=BageC,y=weight))+
  geom_point()+
  coord_cartesian(ylim=c(6.75,7.15))

#get predictions and CI
newdat<-(new_data(weight,terms=c("Bage","cohort[sample=15]",
    "dad_bio [sample=50]","dad_soc [sample=50]","mum [sample=50]"),condition=c(EPb=0,WPb=1)))

newdat<-(new_data(weight,terms=c("Bage"),condition=c(EPb=0,WPb=1)))


newdat1<-as.matrix((expand.grid(Bage=c(1:10),cohort=sample(unique(weightDat$cohort),10),
    dad_bio=sample(unique(weightDat$dad_bio),10),
    dad_soc=sample(unique(weightDat$dad_soc),10),
    mum=sample(unique(weightDat$mum),10),EPb=0,WPb=1,Mage=mean(weightDat$Mage)
    ,Sage=mean(weightDat$Sage),lifespanB=mean(weightDat$lifespanB),
    lifespanS=mean(weightDat$lifespanS),lifespanM=mean(weightDat$lifespanM),
    Jincube=mean(weightDat$Jincube),weight.age=mean(weightDat$weight.age),
    pre1992="no")))
newdat1a<- as.matrix(newdat)
newdat1b<-gpuMatrix(newdat)
##################################
#GPUr
# verify you have valid GPUs
detectGPUs()
# create gpuMatrix and multiply
newdat2 <- vclMatrix(newdat1)

predictions<-predict(weight,newdata=newdat,type="response",se.fit=TRUE,re.form=NA,allow.new.levels=TRUE,options(nbootsim=5))

predictions<-predictInterval(weight,newdata=newdat,which="fixed",n.sims=100)


##########
#non-GPU attempts
#this took overnight to run
#got error: cannot allocate vector of size 5.4 gb (when boots was 500)
#decreasing the RE samples increases the CI alot and also makes for a highly inaccurate line estimate
#test: increase samples t 2-/50 with nbootsim=5


newdat$fit <- cbind(predictions$fit)
newdat$lci <- cbind(predictions$ci.fit[1,])
newdat$uci <- cbind(predictions$ci.fit[2,])

sum.dat<- newdat %>%
  group_by(BageC) %>%
  summarise(fitM=mean(fit),lciM=mean(lci),uciM=mean(uci))
sum.dat$raw.weight <- cbind(weightDat1$weight)
sum.dat$sample <- cbind(weightDat1$num) 


ggplot(data=sum.dat,aes(x=BageC,y=fitM))+
  geom_point(aes(x=BageC,y=raw.weight))+
  geom_line(mapping=aes(x=BageC,y=fitM))+
  geom_ribbon(mapping=aes(x=BageC,ymax=uciM,ymin=lciM),alpha=0.1)+
  theme_classic()
#confidence intervals seem weirdly large here



#################################3
#using arm::sim to calculate CIs

library(arm)

PI.arm.time <- system.time(
  PI.arm.sims <- arm::sim(fm1, 1000)
)

PI.arm <- data.frame(
  fit=apply(fitted(PI.arm.sims, weight), 1, function(x) quantile(x, 0.500)),
  upr=apply(fitted(PI.arm.sims, weight), 1, function(x) quantile(x, 0.975)),
  lwr=apply(fitted(PI.arm.sims, weight), 1, function(x) quantile(x, 0.025))
)

newdat<-new_data(weight,terms=c("BageC","cohort[sample=10]","dad_bio [sample=20]","dad_soc [sample=20]","mum [sample=20]"),condition=c(EPb=0,WPb=1))


arm.sim<-sim(weight, n.sims=1000)



arm.data<- data.frame(
  fit=apply(fitted(PI.arm.sims, weight), 1, function(x) quantile(x, 0.500)),
  upr=apply(fitted(PI.arm.sims, weight), 1, function(x) quantile(x, 0.975)),
  lwr=apply(fitted(PI.arm.sims, weight), 1, function(x) quantile(x, 0.025))
)

arm.data$BageC <- cbind(weightDat$BageC)
arm.data$weightRAW <- cbind(weightDat$weight)

ggplot(data=arm.data,aes(x=BageC,y=fit))+
  geom_smooth(aes(x=BageC,y=fit),method="lm")+
  theme_classic()


sum.arm<- arm.data %>%
  group_by(BageC) %>%
  summarise(weightPRED=mean(fit),lci=mean(lwr),uci=mean(upr),raw.weight=mean(weightRAW))
sum.dat$raw.weight <- cbind(weightDat1$weight)
sum.dat$sample <- cbind(weightDat1$num) 


ggplot(data=sum.arm,aes(x=BageC,y=weightPRED))+
  geom_point(aes(x=BageC,y=raw.weight))+
  geom_line(mapping=aes(x=BageC,y=weightPRED))+
  geom_ribbon(mapping=aes(x=BageC,ymax=uci,ymin=lci),alpha=0.1)+
  theme_classic()














  
  
