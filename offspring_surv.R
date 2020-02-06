library(lmerTest)


offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_bornCOM.csv")
offspringXL <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
########
#offspring to independence
########################

#####
#effect of bio dad age on offspring survival
#####
#linear
inde_survDad<-glmer(independent~Mage+Bage+(1|cohort)+(1|mum)+(1|dad_bio),
                         data=subset(offspring,fate!="unknown"), family="binomial")
summary(inde_survDad)

#quadratric
inde_survDad2<-glmer(independent~poly(Mage,2)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_bio),
                      data=offspring, family="binomial")
summary(inde_survDad2)

inde_survDadm2<-glmer(independent~poly(Mage,2)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_bio),
                    data=offspring, family="binomial")
summary(inde_survDadm2)

inde_survDadd2<-glmer(independent~poly(Mage,1)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_bio),
                      data=offspring, family="binomial")
summary(inde_survDadd2)


#interact with sex
inde_survDad1<-glmer(independent~Bage*sex+(1|cohort)+(1|mum)+(1|dad_bio),
                    data=subset(offspring,sex!="U"), family="binomial")
summary(inde_survDad1)

######
#all parents age effects
######

#survival to independence

#linear
summary(inde_surv<-glmer(independent~Mage+Sage+Bage+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                         data=subset(offspring,EP=="yes"), family="binomial"))

#each term quadratic
summary(inde_survm2<-glmer(independent~poly(Mage,2)+poly(Sage,1)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                         data=subset(offspring,EP=="yes"), family="binomial"))
summary(indesurvm2)

summary(inde_survs2<-glmer(independent~poly(Mage,1)+poly(Sage,2)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                         data=subset(offspring,EP=="yes"), family="binomial"))
summary(indesurvs2)
#############
#in EP matings, young and old social fathers improve offspring survival to independence

summary(inde_survb2<-glmer(independent~poly(Mage,1)+poly(Bage,2)+poly(Sage,1)+(1|cohort)+(1|mum)+(1|dad_bio),
                         data=subset(offspring,EP=="yes"), family="binomial"))
summary(inde_survb2)


#######################################
#fledge
#############

#####
#bio father effects (EP and WP)
#####

#linear
fledge<-glmer(fledged~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
              data=offspring, family="binomial")
summary(fledge)
#positive linear effect of bio dad age approaches signficance
#NB not even close to sig when incube is corrected for

#quad
fledge2<-glmer(fledged~poly(Mage,2)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_bio),
                   data=offspring, family="binomial")
summary(fledge2)
#no quad effects


#######
#(EP and WP)
#######

fledge.soc<-glmer(fledged~Mage+Sage+Jincube+(1|cohort)+(1|mum)+(1|dad_soc)+(1|Nest.ID.mother),
              data=offspring, family="binomial")
summary(fledge.soc)
#NS negative effect of social father's age on offspring survival

#quad
fledge.soc2<-glmer(fledged~poly(Mage,2)+poly(Sage,2)+Jincube+(1|cohort)+(1|mum)+(1|dad_soc)+(1|Nest.ID.mother),
                  data=offspring, family="binomial")
summary(fledge.soc2)
#significant quadratic effect of social father age
#young and old social fathers do better
###NS anymore but possibly because model does not converge

#quad with larger dataset and only soc dad age squared
fledge.soc2<-glmer(fledged~Mage+Sage+I(Sage^2)+Jincube+(1|cohort)+(1|mum)+(1|dad_soc)+(1|Nest.ID.mother),
                   data=offspringXL, family="binomial")
summary(fledge.soc2)
#no effect of the social father here either

fledgeWP.EP<-glmer(fledged~poly(Bage,1)*EP+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                   data=subset(offspring), family="binomial")
summary(fledgeWP.EP)
#NS interaction

fledgeWP.EPs<-glmer(fledged~poly(Sage,2)*EP+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                   data=subset(offspring), family="binomial")
summary(fledgeWP.EPs)
#NS interaction

EP<-glmer(fledged~Mage+Bage+EP+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
          data=subset(offspring), family="binomial")
summary(EP)

#####
#EP only - Sage and Bage effects
######
#linear
fledgeEP<-glmer(fledged~Mage+Sage+Bage+Jincube+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP)
#bio dad is  significant, Highly significant positive effect (p<0.01)

#linear with random effect of nest ID included
fledgeEP<-glmer(fledged~Mage+Sage+Bage+Jincube+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio)+(1|Nest.ID.mother),
                         data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP)
#bio dad age no longer signficant when nest ID controlled for
#however, convergence warning

#interaction between Bage and date
fledgeEP.date<-glmer(fledged~Mage+Sage+Bage*Jincube+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP.date)

#interaction between Sage and Bage
fledgeEPx<-glmer(fledged~Mage+Sage*Bage+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEPx)
#NS positive interaction (p=0.09)
#suggestive that there is a synergistic negative effect of old social and bio dads

#quad
fledgeEP2<-glmer(fledged~poly(Mage,2)+poly(Sage,2)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP2)
#convergence issues

#quad, only Mage
fledgeEP2m<-glmer(fledged~poly(Mage,2)+poly(Sage,1)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                 data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP2m)
#convergence issues

#quad, only Sage
fledgeEP2s<-glmer(fledged~poly(Mage,1)+poly(Sage,2)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                  data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP2s)
#convergence issues
#quad effect of soc age is still approach signficance (p=0.07)
#linear effect of bio dad still signficant

#quad, only Bage
fledgeEP2b<-glmer(fledged~poly(Mage,1)+poly(Sage,1)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                  data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP2b)
#convergence issues
#no quad effect for bio dad

#######
#WP only
####

fledgeWP<-glmer(fledged~poly(Mage,1)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                  data=subset(offspring,EP=="no"), family="binomial")
summary(fledgeWP)
#no effect of paternal age within pair
#the effect is going in the opposite direction (negative)
#this suggests the improved offspring survival with Bage depends not on a difference in sire but a difference in mother



##############
#controlling for lifespan
##############

fledgeEP.long<-glmer(fledged~Mage+Sage+Bage+lifespanB+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP.long)

fledgeEP.long1<-glmer(fledged~Mage+Sage+Bage*lifespanB+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                     data=subset(offspring,EP=="yes"), family="binomial")
summary(fledgeEP.long1)

