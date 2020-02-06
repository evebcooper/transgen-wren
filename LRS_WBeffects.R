#overall age effects
#look at the overall effects of age before we decompose into within and between individual effects

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")


########
#LRS -all
#########

offspring_lrs1<- offspring %>%
  filter(fate=="Died") %>%
  filter(sex!="U")


#####
#WP and EP
####
LRS<-glmmTMB(LRStot~Mage+Bage+
               (1|cohort)+(1|mum)+(1|dad_bio),
             ziformula = ~.,
             data=offspring_lrs1,family=truncated_nbinom1)
summary(LRS)
#no apparent effect of Mage or Bage
#including incubation date does not change results so I removed it

#####
#EP only
#####

LRSep<-glmmTMB(LRStot~Mage+Bage+Sage+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
summary(LRSep)
#no apparent effect of any age, although Sage leans positive and Bage leans negative for conditional part


######
#quantify within and between individual effects
######

#all offspring
LRS.ind<-glmmTMB(LRStot~MageREL+BageREL+MageM+BageM+
               (1|cohort)+(1|mum)+(1|dad_bio),
             ziformula = ~.,
             data=offspring_lrs1,family=truncated_nbinom1)
summary(LRS.ind)
#significant positive effect of maternal mean age (between ind.) in probability of having any LRS

#EP
LRS.indEP<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
summary(LRS.indEP)
#MageM is no longer significant, likely because it's an effect of environment (territory), so SageM is taking some of that effect
#BageREL has a significant negative effect on LRS of individuals that have any LRS

#interaction with sex
LRS.indEPsex<-glmmTMB(LRStot~MageREL+BageREL*sex+SageREL+MageM+BageM+SageM+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
summary(LRS.indEPsex)
#no interaction with sex for father relative age

LRS.indEPsex1<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM*sex+SageM+
                        (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                      ziformula = ~.,
                      data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
summary(LRS.indEPsex1)
#no interaction with sex for father mean age

#difference in between vs. individual effects
#mean age effect represents the difference between between indiv. and within indiv. effects
LRS.indEP.dif<-glmmTMB(LRStot~Mage+Bage+Sage+MageM+BageM+SageM+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
summary(LRS.indEP.dif)
#significant positive BageM, significantly larger effect of between ind. than with-ind.
#means that although males tend to decline with age, older males are in general higher quality 
#this is reflected in our first model of just 'Bage' without controlling for mean age
#the counteracting within-ind and between-ind effects cancel each other out,meaning older males are not better or worse



#####################################################################################
######
#independent offspring
#####
inde<-glmer(independent~Mage+Bage+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
            data=offspring, family="binomial")
summary(inde)
#no effect of Mage or Bage

#does social father's age differ from bio father?
indeEP<-glmer(independent~Mage+Bage+Sage+Jincube+(1|cohort)+(1|dad_soc/mum)+(1|dad_bio),
            data=subset(offspring,EP=="yes"), family="binomial")
summary(indeEP)
#Sage and Bage are both NS and negative


#####
#offspring weight
#####

weight<-lmer(weight~Mage+Bage+Jincube+weight.age+(1|cohort)+(1|mum)+(1|dad_bio),
                 data=offspring)
summary(weight)
#No effect of parental age

#EP only
weightEP<-lmer(weight~Mage+Bage*EP+Jincube+weight.age+(1|cohort)+(1|mum)+(1|dad_bio),
             data=offspring)
summary(weightEP)
#a negative interaction with EP suggests that the age of a father who provides no paternal care influences offspring quality
#more negatively

weightEP2<-lmer(weight~Mage+Bage+Sage+Jincube+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=offspring)
summary(weightEP2)
