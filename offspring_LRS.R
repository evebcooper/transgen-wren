offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
library(glmmTMB)
library(bbmle)
library(sjPlot)

#overall LRS
offspring_lrstot<- offspring %>%
  filter(fate=="Died") %>%
  filter(sex!="U")

hist(offspring_lrstot$LRStot+0.001)
table(offspring_lrstot$LRStot)

#best model with sex interaction (and without) is NB1
NB1lrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                 +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=nbinom1)
LRS.sex<-NB1lrsx
summary(LRS.sex)
#sig interactions: ZI MageREL, ZI BageM

#base model with sig interactions
LRS.b<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
          ziformula = ~MageREL*sex+BageREL+SageREL+MageM+BageM*sex+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring_lrstot,EP=="yes"),family=nbinom1)
summary(LRS.b)
#model doesn't work....
#option 1: re-run model selection on this model and hope to find something that fits
#option 2: run sex-specific models all the way through
#simply report results of full sex interaction model and then report base model

#base model
summary(NB1lrs)
#significant positive effect mean bage on LRS number
#there's no effects in the ZI part of the model...so is it even worth checking early-life effects?

#female-specific LRS effects
summary(NB2lrsf)
#nothing significant
#although there might be sig differences between bage rel and bageM for zero-inflated

#female within- vs. between-individual differences
lrsf.dif<-glmmTMB(LRStot~Mage+Bage+Sage+SageM+MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=nbinom2)
summary(lrsf.dif)
#BageM approaching significance in probability model (p=0.06)
#suggests effect of mean age is positive while effect of relative age is negative 

#male-specific LRS effects
summary(NB1lrsm)
#positive effect of mean Bage on LRS number
#negative effect of BageM on LRS probability

#within vs. between effects for males
lrsm.dif<-glmmTMB(LRStot~Mage+Bage+Sage+SageM+MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=nbinom1)
summary(lrsm.dif)
#sig BageM in both the cond. and ZI parts of model
#sig MageM in ZI

#table results
tab_model(NB1lrsx,NB2lrsf,lrsf.dif,NB1lrsm,lrsm.dif,transform=c(NULL),show.icc=FALSE,
          title = "Lifetime Reproductive Success",
          dv.labels =c( "All offspring","Females", "Females within VS. between","Males", "Males within VS. between"))

tab_model(NB2lrsf,NB1lrsm,transform=c(NULL),show.icc=FALSE,
          title = "Lifetime Reproductive Success",
          dv.labels =c( "Females","Males"))

tab_model(lrsf.dif,lrsm.dif,transform=c(NULL),show.icc=FALSE,
          title = "LRS: Within vs. Between-Individual Differences",
          dv.labels =c( "Females","Males"))

tab_model(NB1lrsx,transform=c(NULL),show.icc=FALSE,
          title = "LRS: Sex-specific effects")



#######################################################

#####
#WP and EP
####
#Bage x EP interaction
LRS.all<-glmmTMB(LRStot~Mage+Bage*EP+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=offspring_lrstot,family=nbinom1)
summary(LRS.all)
#probability: positive effect of Bage  lessened when father is extra-pair (p=0.03)
#conditional: no significant effects

#sage x EP interaction
LRS.allS<-glmmTMB(LRStot~Mage+Sage*EP+Bage+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=offspring_lrstot,family=nbinom1)
summary(LRS.allS)
#probability: positive effect of Sage lessened when father is extra-pair
#conditional: no significant effects

#WP and EP -males only
LRS.m<-glmmTMB(LRStot~MageREL+MageM+BageREL*EP+BageM*EP+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_lrstot,sex=="M"),family=nbinom1)
summary(LRS.m)
#EP interacts with BageM


#WP and EP -males only -absolute age
LRS.mA<-glmmTMB(LRStot~Mage+Bage*EP+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,
               data=subset(offspring_lrstot,sex=="M"),family=nbinom1)
summary(LRS.mA)
#could not calculate 

#WP and EP -females only
LRS.f<-glmmTMB(LRStot~MageREL+MageM+BageREL*EP+BageM*EP+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,
               data=subset(offspring_lrstot,sex=="F"),family=nbinom2)
summary(LRS.f)
#NS interactions

#WP and EP -females only - absolute age
LRS.fA<-glmmTMB(LRStot~Mage+Bage*EP+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,
               data=subset(offspring_lrstot,sex=="F"),family=nbinom2)
summary(LRS.fA)
#NS interactions


######
#WP only
######

#females
LRS.WPf<-glmmTMB(LRStot~MageREL+BageREL+MageM+BageM+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~.,
                  data=subset(offspring_lrstot,EP=="no"&sex=="F"),family=nbinom2)
summary(LRS.WPf)
#no sig effects
#BageM skews negative for both
#BageREL skew positive for ZI

#males
LRS.WPm<-glmmTMB(LRStot~MageREL+BageREL+MageM+BageM+(1|cohort)+(1|mum)+(1|dad_bio),
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="no"&sex=="M"),family=nbinom1)
summary(LRS.WPm)
#Bage M has sig positive effect on ZI part of model


