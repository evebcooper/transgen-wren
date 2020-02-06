#####
#load packages
#####
library(tidyr)
library(dplyr)
library(lmerTest)
library(ggplot2)
library(rcompanion)
library(gridExtra)
library(readr)
library(sjPlot)

#####
#bring in data
#####
##add data of all birds born or caught ever
all_birds <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/data/all_birdsUPDATED.csv", header=FALSE)

#add data headers
#NB that "death" is actually date of disappearance for birds we don't know
#e.g. juvenile females
names(all_birds)<-c("ID","band_num","cohort","caught_as","first_caught","death","fate","unknown",
                    "fledged","independent", "mum","dad_soc","dad_bio","dad_bioALT","sex")

#####
#data prep
#####

#remove weird characters from ID related columns
all_birds$ID<-gsub(pattern="ï»¿", replacement="",all_birds$ID)
all_birds$ID<-gsub(pattern="﻿", replacement="",all_birds$ID)
all_birds$mum<-gsub(pattern="ï»¿", replacement="",all_birds$mum)
all_birds$mum<-gsub(pattern="﻿", replacement="",all_birds$mum)
all_birds$dad_soc<-gsub(pattern="ï»¿", replacement="",all_birds$dad_soc)
all_birds$dad_soc<-gsub(pattern="﻿", replacement="",all_birds$dad_soc)
all_birds$dad_bio<-gsub(pattern="ï»¿", replacement="",all_birds$dad_bio)
all_birds$dad_bio<-gsub(pattern="﻿", replacement="",all_birds$dad_bio)

#create new dataset with only individuals born on study site
study_born <- all_birds %>%
  filter(caught_as == "Nestling"|caught_as=="Fledgling") %>%
  #change unknow moms/dads/soc dads to NA
  mutate(dad_bio=na_if(dad_bio,"*")) %>%
  mutate(dad_bio=na_if(dad_bio,"")) %>%
  mutate(dad_bio=na_if(dad_bio,"epc")) %>%
  mutate(dad_bio=na_if(dad_bio,"epc-NS")) %>%
  mutate(dad_soc=na_if(dad_soc,"*")) %>%
  mutate(dad_soc=na_if(dad_soc,"")) %>%
  mutate(mum=na_if(mum,"*")) %>%
  mutate(mum=na_if(mum,"")) %>%
  #add in a year identifier for each parent
  mutate(dad_soc.yr = ifelse(is.na(dad_soc),NA, paste(dad_soc,cohort,sep='--'))) %>%
  mutate(dad_bio.yr = ifelse(is.na(dad_bio),NA,paste(dad_bio,cohort,sep='--'))) %>%
  mutate(mum.yr = ifelse(is.na(mum),NA,paste(mum,cohort,sep='--')))
###########################################################################################
#bring in female and male mortality data in order to get ages
morFall <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/data/morFsept.csv")

morFall <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/wren_age_vs_traits/morFall.csv")
morMall <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/wren_age_vs_traits/morMall.csv")

#simplify the columns and change column names so the data files match
morFall1<-morFall %>%
  select(hen.yr,henID,year,cohort,age) %>%
  rename(ID.yr=hen.yr,ID=henID) %>%
  mutate(sex="F")

morMall1<-morMall %>%
  select(ID.yr,ID,year,cohort,age)%>%
  mutate(sex="M")

#bring the mortality dataframes into one
mor_all <- rbind(morFall1,morMall1)

###################################################################################################
#create datafile
write.csv(mor_all,file="mor_all.csv")
###################################################################################################


#left join the new mor_all with study born by parents in order to add ages
study_born1 <- left_join(study_born,select(mor_all,c(age,ID.yr)),by=c("dad_soc.yr"="ID.yr")) %>%
  rename(dad_soc_age=age)
study_born <- left_join(study_born,select(mor_all,c(age,ID.yr)),by=c("dad_bio.yr"="ID.yr")) %>%
  rename(dad_bio_age=age)
study_born <- left_join(study_born,select(mor_all,c(age,ID.yr)),by=c("mum.yr"="ID.yr")) %>%
  rename(mum_age=age)  

#some mums and dads will be missing because they are unknown age 
#even more bio_dads will be missing due to being unknown

#######################################################################################

parentDat<-all_birds %>%
  rename(cohortPar=cohort) %>%
  group_by(ID)%>%
  summarise(cohortPar=cohortPar)

#add to study born
study_born <- left_join(study_born,select(parentDat,c(ID,cohortPar)),by=c("dad_soc"="ID")) %>%
  rename(dad_soc_cohort=cohortPar)
study_born <- left_join(study_born,select(parentDat,c(ID,cohortPar)),by=c("dad_bio"="ID")) %>%
  rename(dad_bio_cohort=cohortPar)
study_born <- left_join(study_born,select(parentDat,c(ID,cohortPar)),by=c("mum"="ID")) %>%
  rename(mum_cohort=cohortPar)


#add column to denote when bio_dad == soc_dad

study_born<-study_born %>%
  mutate(sameDad=ifelse(dad_bio==dad_soc,"yes","no"))



#####
#analysis
#####

################
#does mom and dad age correlate?
##################

summary(lmer(mum_age~dad_bio_age+(1|dad_bio),data=study_born))
cor.test(study_bornSUB$mum_age,study_bornSUB$dad_soc_age,method = "pearson")
#social pair's ages are positively correlated

cor.test(study_bornEP$mum_age,study_bornEP$dad_bio_age,method = "pearson")
#no correlation between mum's age and EP father age

cor.test(study_bornEP$dad_soc_age,study_bornEP$dad_bio_age,method = "pearson")
#no correlation between social father's age and genetic father age in EP offspring

cor.test(study_born$dad_soc_age,study_born$dad_bio_age,method = "pearson")
#there is a correlation between soc and bio dad age (since sometimes the same bird)


####################
#independent
####################

#probability of making to independent
summary(inde_surv<-glmer(independent~mum_age+dad_bio_age+dad_soc_age+(1|cohort),
                        data=study_born, family="binomial"))
#none of the ages are significant

#lets properly add in random effects just to see
summary(inde_surv<-glmer(independent~mum_age+dad_bio_age+dad_soc_age+(1|cohort)
                         +(1|mum)+(1|dad_soc)+(1|dad_bio),
                         data=study_born, family="binomial"))

#without dad_bio since this considerably reduces sample size
summary(inde_surv<-glmer(independent~mum_age+dad_soc_age+(1|cohort)
                         +(1|mum)+(1|dad_soc),
                         data=study_born, family="binomial"))
#nope

#interact all factors
summary(inde_surv<-glmer(independent~mum_age*dad_bio_age*dad_soc_age+(1|cohort),
                         data=study_born, family="binomial"))
#model convergence issues
#mum and soc dad interaction approaches sig

summary(inde_surv<-glmer(independent~mum_age*dad_soc_age+(1|cohort),
                         data=study_born, family="binomial"))
#no longer significant

#excluding 1 year old mothers
summary(inde_surv<-glmer(independent~mum_age+dad_bio_age+dad_soc_age+(1|cohort),
                         data=study_born[study_born$mum_age>1,], family="binomial"))

#quadratic effect?
summary(inde_surv<-glmer(independent~I(mum_age^2)+I(dad_soc_age^2)+I(dad_bio_age^2)+
                           (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                         data=study_born, family="binomial"))

summary(inde_surv<-glmer(independent~I(mum_age^2)+mum_age+I(dad_soc_age^2)+dad_soc_age+
                           (1|cohort)+(1|mum)+(1|dad_soc),
                         data=study_born, family="binomial"))

summary(inde_surv<-glmer(independent~poly(dad_bio_age,2)+(1|cohort)+(1|mum),
                         data=subset(study_born,!is.na(dad_bio_age)), family="binomial"))

summary(inde_surv<-glmer(independent~poly(dad_soc_age,2)+(1|cohort)+(1|mum)+(1|dad_soc),
                         data=subset(study_born,!is.na(dad_soc_age)), family="binomial"))
#significant quadratic effect
#young and old fathers have higher offspring survival

summary(inde_surv<-glmer(independent~poly(dad_soc_age,1)+(1|cohort)+(1|mum)+(1|dad_soc),
                         data=subset(study_born,!is.na(dad_soc_age)), family="binomial"))
#linear effect approaching signficiance (p=0.09)
#older fathers have less offspring survival


summary(inde_surv<-glmer(independent~poly(mum_age,2)+(1|cohort)+(1|mum)+(1|dad_soc),
                         data=subset(study_born,!is.na(mum_age)), family="binomial"))
#same quad effect as dad, young and old mothers have higher offspring survival

summary(inde_surv<-glmer(independent~poly(mum_age,1)+(1|cohort)+(1|mum)+(1|dad_soc),
                         data=subset(study_born,!is.na(mum_age)), family="binomial"))
#linear effect is NS in mums


summary(inde_surv<-glmer(independent~poly(dad_bio_age,2)+(1|cohort)+(1|mum)+(1|dad_bio),
                         data=subset(study_born,!is.na(dad_bio_age)), family="binomial"))
#no effect of bio dad

study_bornSUB<- study_born %>%
  filter(!is.na(dad_soc_age)) %>%
  filter(!is.na(mum_age))
summary(inde_surv<-glmer(independent~poly(mum_age,2)+poly(dad_soc_age,2)+(1|cohort)+(1|mum)+(1|dad_soc),
                        data=study_bornSUB , family="binomial"))
#quad effects of soc dad and mum are robust to both being in the model


############################################################
#fledge
############################################


##parents age effect on surviving to fledged
summary(fledge_surv<-glmer(fledged~mum_age+dad_bio_age+dad_soc_age+
                             (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                         data=study_born, family="binomial"))
#older bio dads improve survivorship of offspring!!!

##however excluding soc dad or mum takes away signficance
#make a df excluding individuals where social dad = bio dad and try analysis again

summary(fledge_surv<-glmer(fledged~dad_bio_age+I(dad_bio_age)^2+dad_soc_age+
                             (1|cohort)+(1|dad_bio),
                           data=subset(study_born), family="binomial"))


#interactive effects
summary(fledge_surv<-glmer(fledged~mum_age*dad_bio_age*dad_soc_age+
                             (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                           data=study_born, family="binomial"))
#convergences issues

#exclude bio dad
summary(fledge_surv<-glmer(fledged~mum_age*dad_soc_age+
                             (1|cohort)+(1|mum)+(1|dad_soc),
                           data=study_born, family="binomial"))
#no effects

summary(fledge_surv<-glmer(fledged~mum_age+I(mum_age^2)+dad_soc_age+I(dad_soc_age^2)+
                             (1|cohort)+(1|mum)+(1|dad_soc),
                           data=study_born, family="binomial"))


#quadratic effects
summary(fledge_surv<-glmer(fledged~I(mum_age^2)+I(dad_bio_age^2)+I(dad_soc_age^2)+
                             (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                           data=study_born, family="binomial"))
#####
#EP offspring only
#####

#create data subset where the bio dad and soc dad are different
#i.e. cases of EPP

study_bornEP <- study_born %>%
  filter(dad_soc!=dad_bio)


##parents age effect on surviving to fledged
summary(fledge_survEP<-glmer(fledged~mum_age+dad_bio_age+dad_soc_age+
                             (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                           data=study_bornEP, family="binomial"))
#when cases where the bio and soc dad are the same are removed,
#bio age becomes only marginally sig

#independent
summary(inde_survEP<-glmer(independent~mum_age+dad_bio_age+dad_soc_age+
                               (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                             data=study_bornEP, family="binomial"))
#on indepedent survival bio dad actually has a negative (opposite) effect
#but not significant at all

#interactions
summary(fledge_survEP<-glmer(fledged~mum_age*dad_bio_age+
                               (1|cohort)+(1|mum)+(1|dad_bio),
                             data=study_bornEP, family="binomial"))
#nope 

summary(fledge_survEP<-glmer(fledged~dad_soc_age*dad_bio_age+
                               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                             data=study_bornEP, family="binomial"))
#nope


#quadratic effects
summary(fledge_survEP<-glmer(fledged~I(mum_age*mum_age)+dad_bio_age+I(dad_bio_age^2)+
                               (1|cohort)+(1|mum)+(1|dad_bio),
                             data=study_bornEP, family="binomial"))
summary(inde_survEP<-glmer(independent~I(mum_age^2)+I(dad_bio_age^2)+(dad_soc_age^2)+
                               (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                             data=study_bornEP, family="binomial"))
#suggest of quadratic effect of dad bio age although not quite significant,
#only for fledge, not independent

study_bornSUBtot<- study_bornSUB%>%
  filter(!is.na(dad_bio_age))

summary(fledge_survEP<-glmer(fledged~poly(mum_age,1)+poly(dad_bio_age,1)+poly(dad_soc_age,1)+
                               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                             data=subset(study_bornSUBtot,sameDad!="yes"), family="binomial"))

summary(fledge_survEP<-glmer(fledged~poly(mum_age,2)+poly(dad_bio_age,2)+poly(dad_soc_age,2)+
                               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                             data=subset(study_bornSUBtot,sameDad!="yes"), family="binomial"))

#removing dad soc
summary(fledge_survEP<-glmer(fledged~I(mum_age^2)+I(dad_bio_age^2)
                             +(1|cohort)+(1|mum)+(1|dad_bio),
                             data=study_bornEP, family="binomial"))
#without the social dad bio dad quad is signficant

#does this pattern hold for indpendent young
summary(fledge_survEP<-glmer(independent~I(mum_age^2)+I(dad_bio_age^2)
                             +(1|cohort)+(1|mum)+(1|dad_bio),
                             data=study_bornEP, family="binomial"))
#no


#graph
ggplot(study_born,aes(dad_bio_age,fledged)) +
  stat_summary()
ggplot(study_born,aes(dad_bio_age,independent)) +
  stat_summary()

ggplot(study_born,aes(dad_soc_age,fledged)) +
  stat_summary() 

ggplot(study_born,aes(mum_age,fledged)) +
  stat_summary() 

ggplot(study_born,aes(mum_age,independent)) +
  stat_summary()
summary(parent_ages<-glmer(dad_soc_age~dad_bio_age+(1|cohort),data=study_bornEP,family="poisson"))











#####
#egg to banding survival
#####

#import nest file
nests <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/wren_age_vs_traits/nest_fileCLEAN.csv")

#data needed: from the nest data,
#No.of.eggs - No.hatched (or No.fledged)
#the cleaned nest data only includes nests that had eggs (good)

egg_surv<- nests %>%
  select(nestSeason,motherID,motherAge,domID,domAge,
         eggs,hatched,fledged,fledgedB,daysSurvfromIncube, independent,
         numInfertile,numLegitimate,numPaternityUnknown,socialIncest) %>%
  rowwise()%>%
  mutate(hatch_ratio= (hatched/eggs)) %>%
  mutate(fledge_ratio= (fledged/eggs)) %>%
  mutate(inde_ratio=(independent/eggs)) %>%
  mutate(indeH_ratio=ifelse(hatched==0,NA,(independent/hatched))) %>%
  mutate(fledgeH_ratio=ifelse(hatched==0,NA,(fledged/hatched))) %>%
  mutate(indeF_ratio=ifelse(fledged==0,NA,(independent/fledged))) %>%
  filter(!fledge_ratio>1)
#change estimated ages of dom male to NA
egg_surv$domAge<- as.numeric(as.character(egg_surv$domAge))
#remove ages that are obvious errors
egg_surv<-egg_surv[egg_surv$domAge>0 & egg_surv$domAge<14,]
#remove rows that are all NA
egg_surv <- egg_surv %>% filter_all(any_vars(!is.na(.)))




#####
#models
#####
#models must be weighted by the number of eggs ("weights" argument)
#i.e a proportion of 0.5 coming from a clutch of 2 has half the weighting of,
# the same proportion (0.5) coming from a clutch of 4,
#since it represents half the data

#####
#egg -> hatch
#####

#proportion of eggs to hatch
summary(egg_hatch<-glmer(hatch_ratio~poly(motherAge,2)+(1|nestSeason)+(1|motherID),
                         data=egg_surv,family="binomial",weights=eggs))
#significant negative linear effect
#older mothers negatively influence hatch success
# no quadratic effect
#this sample excludes females that had 0 eggs (and all subsequent analysis)




#####
#hatch -> fledge
#####

summary(hatch_to_fledge<-glmer(fledgeH_ratio~poly(motherAge,2)+(1|nestSeason)+(1|motherID),
                               data=egg_surv,family="binomial",weights=hatched))
##no linear or quadratic effect

#add in males
summary(hatch_to_fledge1<-glmer(fledgeH_ratio~poly(motherAge,2)+poly(domAge,2)+
                                 (1|nestSeason)+(1|motherID)+(1|domID),
                               data=egg_surv,family="binomial",weights=hatched))
##no effects




#####
#fledge -> independent
#####

summary(fledge_to_inde<-glmer(indeF_ratio~poly(motherAge,2)+(1|nestSeason)+(1|motherID),
                              data=egg_surv,family="binomial",weights=hatched))
##positive quadratic
#mothers are better at keeping offspring alive at young and old (but not intermediate) ages

#adding in males
summary(fledge_to_inde1<-glmer(indeF_ratio~poly(motherAge,2)+poly(domAge,2)+
                                (1|nestSeason)+(1|motherID)+(1|domID),
                              data=egg_surv,family="binomial",weights=hatched))
##no effect of male age
#female quadratic effect is no longer significant

#####
#table results
#####

stargazer(egg_hatch,hatch_to_fledge1,fledge_to_inde1,
 dep.var.labels=c("Egg to hatchling","Hatchling to fledgling", "Fledgling to independence"),
 covariate.labels = c("Mother age","Mother age (quad)","Soc. father age","Soc. father age (quad)"),
 single.row=FALSE,omit.stat=c("LL"),report=('vcsp*'),
          align=TRUE,type="text")

tab_model(egg_hatch,hatch_to_fledge1,fledge_to_inde1,
          show.est=TRUE,show.std="std",
          show.icc=FALSE,exp.coef=FALSE)
sjt.glmer(egg_hatch,hatch_to_fledge1,fledge_to_inde1,
      pred.labels=c("Mother age","Mother age (quad)","Soc. father age","Soc. father age (quad)"),
      depvar.labels = c("Egg to hatchling","Hatchling to fledgling", "Fledgling to independence"),
          exp.coef=FALSE,emph.p=TRUE,show.re.var=TRUE,string.est="Estimate",
      file="offspring_surv1.htm")

#####
#hatch -> independent
#####
##this spans both the time spans of the above two models
#just used to double check that the fine timescale isn't hiding any effect in above models
summary(hatch_to_inde<-glmer(indeH_ratio~poly(motherAge,2)+(1|nestSeason)+(1|motherID),
                             data=egg_surv,family="binomial",weights=hatched))
##no linear or quadratic effect

#males
summary(hatch_to_inde<-glmer(indeH_ratio~poly(domAge,2)+poly(motherAge,2)+
                               (1|nestSeason)+(1|domID)+(1|motherID),
                             data=egg_surv,family="binomial",weights=hatched))
#no effect


#####
#egg -> fledge
#####
#11 days

###proportion of eggs to fledge
summary(egg_fledge<-glmer(fledge_ratio~poly(motherAge,2)+(1|nestSeason)+(1|motherID),
                         data=egg_surv,family="binomial",weights=eggs))
#marginal negative linear effect
#no quadratic effect

#adding in social father age
summary(egg_fledge<-glmer(fledge_ratio~domAge+(1|nestSeason)+
                            (1|domID),
                          data=egg_surv,family="binomial",weights=eggs))
#no linear effect

summary(egg_fledge<-glmer(fledge_ratio~I(domAge^2)+(1|nestSeason)+
                            (1|domID),
                          data=egg_surv,family="binomial",weights=eggs))
#and no quadractic effect




#####
#independent
#####

###proportion of eggs to independent
summary(egg_inde<-glmer(inde_ratio~motherAge+(1|nestSeason)+(1|motherID),
                          data=egg_surv,family="binomial",weights=eggs))
#mother age becomes non-significant beyond fledging 