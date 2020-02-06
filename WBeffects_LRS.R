#overall age effects
#look at the overall effects of age before we decompose into within and between individual effects

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")


########
#LRS -all
#########

offspring_lrs1<- offspring %>%
  filter(fate=="Died") %>%
  filter(sex!="U")

#best model chosen in ZI_model_selection.R
#NB1lrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
        #ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=nbinom1)


#####
#WP and EP
####
LRS<-glmmTMB(LRStot~Mage+Bage+
               (1|cohort)+(1|mum)+(1|dad_bio),
             ziformula = ~.,
             data=offspring_lrs1,family=nbinom1)
summary(LRS)
#no apparent effect of Mage or Bage
#including incubation date does not change results so I removed it

#####
#EP only
#####

LRSep<-glmmTMB(LRStot~Mage+Bage+Sage+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
summary(LRSep)
#in conditional model, there is a positive effect of Sage


######
#quantify within and between individual effects
######

#all offspring
LRS.ind<-glmmTMB(LRStot~MageREL+BageREL+MageM+BageM+
                   (1|cohort)+(1|mum)+(1|dad_bio),
                 ziformula = ~.,
                 data=offspring_lrs1,family=nbinom1)
summary(LRS.ind)
#significant positive effect of maternal mean age (between ind.) in probability of having any LRS

#EP
LRS.indEP<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
summary(LRS.indEP)
#positive effect of mean Bage for LRS number
#NS negative effect of relative Bage for LRS number


#EP - sex interactions
LRS.indEPsex<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
summary(LRS.indEPsex)


#EP - Bage only
LRS.indEPb<-glmmTMB(LRStot~MageREL+BageREL+MageM+BageM+
                     (1|cohort)+(1|mum)+(1|dad_bio),
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
summary(LRS.indEPb)



#interaction with sex
LRS.indEPsex<-glmmTMB(LRStot~MageREL+BageREL*sex+SageREL+MageM+BageM+SageM+
                        (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                      ziformula = ~.,
                      data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
summary(LRS.indEPsex)
#no interaction with sex for father relative age

LRS.indEPsex1<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM*sex+SageM+
                         (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                       ziformula = ~.,
                       data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
summary(LRS.indEPsex1)
#zero-inflated:
#mean age of bio dad has a less positive effect on the probability of LRS for male offspring

#females only
LRS.indEPfem<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                         (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                       ziformula = ~.,
                       data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=nbinom1)
summary(LRS.indEPfem)
#no significant effects
#NS positive effect of mean bage on probability of LRS

#males only
LRS.indEPm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                        (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                      ziformula = ~.,
                      data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=nbinom1)
summary(LRS.indEPm)
#negative effect of mean Bage on probability of LRS
#negative effect of relative Mage on probability of LRS
#positive effect of mean Bage on LRS number 

#difference in between vs. individual effects
#mean age effect represents the difference between between indiv. and within indiv. effects
LRS.indEP.dif<-glmmTMB(LRStot~Mage+Bage+Sage+MageM+BageM+SageM+
                         (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                       ziformula = ~.,
                       data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
summary(LRS.indEP.dif)
#significant positive BageM, significantly larger effect of between ind. than with-ind.
#means that although males tend to decline with age, older males are in general higher quality 
#this is reflected in our first model of just 'Bage' without controlling for mean age
#the counteracting within-ind and between-ind effects cancel each other out,meaning older males are not better or worse



#####################################################################################
