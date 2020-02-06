offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
#mother-son social pairings are removed
#EP offspring sired by a helper on the territory are denoted by sire = WG

#####
#independence

#base - no incube
indeD<-glmer(independent~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+sex+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,sex!="U"), family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(indeD)
#postive effect of WP Bage becomes NS (p=0.08) when you exclude Jincube




#add helpers
inde.sep1<-glmer(independent~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring, family="binomial")
ss <- getME(inde.sep1,c("theta","fixef"))
inde.sep1<- update(inde.sep1,start=ss,control=glmerControl(optCtrl=list(maxfun=8e4)))
summary(inde.sep1)
#still sig positive effect of WP dad, suggesting it's not simply older soc dads are more likely to have help
#converged

#helper interaction with WP Bage
inde.sep1a<-glmer(independent~Mage+EPb+Sage:EPb+Bage:WPb:help.cat+Jincube+help.cat+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.sep1,c("theta","fixef"))
inde.sep1<- update(inde.sep1,start=ss,control=glmerControl(optCtrl=list(maxfun=10e10)))
summary(inde.sep1)


#swapping base coefficient of 'EPb' to 'WPb' to see if it makes a difference
inde.sepOP<-glmer(independent~Mage+WPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.sepOP,c("theta","fixef"))
inde.sepOP<- update(inde.sepOP,start=ss,control=glmerControl(optCtrl=list(maxfun=10e10)))
summary(inde.sepOP)
#results are the same

#removing base coefficients of 'EPb'/'WPb' to see if it makes a difference
inde.sepOP1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=offspring, family="binomial")
ss <- getME(inde.sepOP1,c("theta","fixef"))
inde.sepO1P<- update(inde.sepOP1,start=ss,control=glmerControl(optCtrl=list(maxfun=10e10)))
summary(inde.sepOP1)
#no significance anymore
#estimate = 0.0513 + 0.0312
#the estimate has become smaller

#excluding helper-mother extra-pairings
inde.noSon<-glmer(independent~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sire!="WG"), family="binomial")
ss <- getME(inde.noSon,c("theta","fixef"))
inde.noSon<- update(inde.noSon,start=ss,control=glmerControl(optCtrl=list(maxfun=8e4)))
summary(inde.noSon)
#results hold
#model converged

#difference in fitness between EP WG and WP
inde.noSon<-glmer(independent~sire+help.cat+Jincube+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=offspring, family="binomial")
summary(inde.noSon)

#inde seperated by REL and mean
#including/excluding Jincube does not change convergence issues (keep in)
inde.sep2<-glmer(independent~MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+Jincube+
                  SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring, family="binomial")
ss <- getME(inde.sep2,c("theta","fixef"))
inde.sep2<- update(inde.sep2,start=ss,control=glmerControl(optCtrl=list(maxfun=14e14)))
summary(inde.sep2)
#positive effect of WP S/Bage is driven by improvement with relative age (experience theory)
#no effect of BageREL (ep) indicates that there is no sperm deterioration with age
#neg effect of mean Sage in EP, indicates males that live longer  are more dicerning of not their offspring
#NS neg effects of both BageM and BageRel

#######
#weight


#excluding helpers
weight.sep<-lmer(weight/weight.age~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+sex+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sex!="U"))
ss <- getME(weight.sep,c("theta","fixef"))
weight.sep<- update(weight.sep,start=ss,control=lmerControl(optCtrl=list(maxfun=8e8)))
summary(weight.sep)
#significant negative effect of EP Bage
#WP father positive effect approaches significance (p=0.07)

#including helpers
weight.sep1<-lmer(weight~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+Jincube+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,sire!="WG"),REML=FALSE)
summary(weight.sep1)
#NS effect of WP father when helpers are controlled for
#suggests that part of the reason having an old WP dad is good is it means there are helpers more likely

#excluding EPb from base
weight.sep1a<-lmer(weight/weight.age~WPb+Mage+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+Jincube+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=offspring1)
summary(weight.sep1a)
#when we remove EPb from base it is constraining all interactions to have the same intercept
#when we do this, all results become non-significant 
#the negative effect of EPb and the 

#excluding mother-helper pairings
weight.noSon<-lmer(weight/weight.age~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+Jincube+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=offspring_noSon)
summary(weight.noSon)
#results still hold

#add mother-helper pairing as fixed effect
weight.ephelp<-lmer(weight/weight.age~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+Jincube+
                     son.ep+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   data=offspring)
summary(weight.ephelp)
#no effect of extra-pair mating being by a helper

#weight.sep seperated for rel and mean
weight.sep2<-lmer(weight/weight.age~MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+
                    SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+help.cat+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring)
summary(weight.sep2)
#positive effect of WP S/Bage is driven by improvement with relative age (experience theory)
#no effect of BageREL (ep) indicates that there is no sperm deterioration with age
#NS neg (p=0.09) effect of mean Sage in EP, indicates males that live longer  are more dicerning of not their offspring
#NS neg effects of both BageM and BageRel
###################################################
#including sex, brood size, and pre-1992

#base model
offspring<-filter(offspring, sex!="U")

weight<-lmer(weight~Mage+EPb+help.cat+Bage:EPb+Sage:EPb+Bage:WPb+weight.age+pre1992+Jincube+sex+No.hatched+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sire!="WG"),control=lmerControl(optimizer ="bobyqa"))
summary(weight)
#negative effect of EP Bage is no longer significant (p=0.06)

weightRM<-lmer(weight~weight.age+MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+
                    SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+help.cat+Jincube+
                 pre1992+No.hatched+sex+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=lmerControl(optimizer ="bobyqa"),
                  data=offspring)
summary(weightRM)

#################################################


weight.int<-lmer(independent~Mage+Sage*Bage+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,EP=="yes"))
summary(weight.int)


weight.h<-lmer(independent~Mage+Sage+Bage+weight.age+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,EP=="yes"))
summary(weight.h)
#no sig effects

weight.h2<-lmer(independent~Mage+Sage+Bage+weight.age+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=offspring)
summary(weight.h2)
#no sig effects

weight.h3<-lmer(independent~Mage+Sage*EP+Bage+weight.age+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring)
summary(weight.h3)
#still sig interaction between Sage and EP (p = 0.003)

weight.h4<-lmer(independent~Mage+Sage+Bage*EP+weight.age+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring)
summary(weight.h4)
#still sig interaction between Bage and EP



