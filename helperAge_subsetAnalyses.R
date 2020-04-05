#help num vs category vs binary
#ALSO: subset to only helped, only helped by one, help age as a factor


offspring <- offspring %>%
  mutate(helpSonCat=ifelse(helpSonNum==0,"none",ifelse(helpSonNum==1,"one","twoPlus")))%>%
  mutate(helpUnrelateCat=ifelse(helpUnrelateNum==0,"none",ifelse(helpUnrelateNum==1,"one","twoPlus")))

####################################
#weight
#help binary
summary(weight.ha1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                           lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+
                           helpMeanAge:helpB+helpSonB+helpUnrelateB+
                           (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                         data=offspring,control=lmerControl(optimizer ="bobyqa")))
summary(weight.ha1)


#weight
#help cat
summary(weight.ha2<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                           lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpSonCat+helpUnrelateCat+helpMeanAge:helpB+
                           (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                         data=offspring,control=lmerControl(optimizer ="bobyqa")))
#having two plus son helpers is marginally good
#no effect of helper mean age

#help number
summary(weight2<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                           lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+
                           helpMeanAge:helpB+helpUnrelateNum+helpSonNum+
                           (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                         data=offspring,control=lmerControl(optimizer ="bobyqa")))
summary(weight2)
# help son num is positive but NS (p=0.06)


#subset to different helper conditions
#1 helper, helper age as factor
###################################
#independence

#helper category
inde.son6<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son6)
#negative effect of having one unrelated helper
#help age effect is the same as in binary 

#subset to only individuals that had help
inde.helponly<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge+helpSonB+helpUnrelateB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=subset(offspring,helpB==1), family="binomial")
summary(inde.helponly)
#help age effect is now 0.14 (down from 0.21)


#subset only one helper (n=1131)
#exclude helpunrelatB (this is always going to be opposite help son)
inde.1help<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge+helpSonB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=subset(offspring,helpNum==1), family="binomial")
summary(inde.1help)
#convergence issues
#no effect of help
#coefficient = 0.15

#subset only one  or no helpers
#exclude helpunrelatB (this is always going to be opposite help son)
inde.1help2<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                    (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                  data=subset(offspring,helpNum<2), family="binomial")
summary(inde.1help2)
#no effect
#coefficient = 0.16

#remove dad_soc
inde.1help1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge+helpSonB+
                    (1|mum)+(1|cohort)+(1|dad_bio),control=glmerControl(optimizer ="bobyqa"),
                  data=subset(offspring,helpNum==1), family="binomial")
summary(inde.1help1)
#still convergence issues

#exclude parental lifespans
inde.1help2<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+
                    EPb+Jincube+helpMeanAge+helpSonB+
                    (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                  data=subset(offspring,helpNum==1), family="binomial")
summary(inde.1help2)
#singular fit
#unable to estimate cohort random effect

#exclude all non helper effects
inde.1help3<-glmer(independent~helpMeanAge+helpSonB+
                     (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                   data=subset(offspring,helpNum==1), family="binomial")
summary(inde.1help3)
#model does converge
#no sig effects

#exclude all non helper effects that are non significant in previous model
inde.1help4<-glmer(independent~helpMeanAge+helpSonB+Mage+lifespanM+Jincube+Bage:WPb+WPb+
                     (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                   data=subset(offspring,helpNum==1), family="binomial")
summary(inde.1help4)
#model does converge
#no sig effects of helper age
############################

#simplify to a non RE model and see how adding terms impacts model
summary(indesim<-glm(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat,
                 data=offspring, family="binomial"))
#helper age = 0.09


#remove mother age terms
summary(indesim<-glm(independent~Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                       EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat,
                     data=offspring, family="binomial"))
#helper age = 0.06

#remove Bage terms
summary(indesim<-glm(independent~Mage+lifespanM+Sage:WPb+
                       EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat,
                     data=offspring, family="binomial"))
#helper age = 0.03

#remove everything
summary(indesim<-glm(independent~helpMeanAge:helpB+helpSonCat+helpUnrelateCat,
                     data=offspring, family="binomial"))
#helper age = 0.01


###########
#helper age as a factor

inde.helpFACt<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                      EPb+Jincube+as.factor(helpMeanAge):helpB+helpSonCat+helpUnrelateCat+
                      (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                    data=subset(offspring), family="binomial")
summary(inde.helpFACt)
#dropped a column () due to rank deficient

#subset to only one helper
inde.helpFAC<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+as.factor(helpMeanAge)+helpSonCat+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=subset(offspring,helpNum==1), family="binomial")
summary(inde.helpFAC)
#converge issues
#having 2 helpers is worse
#no sig difference for other number of helpers

#subset to only one helper or no helpers
inde.helpFACa<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                      EPb+Jincube+as.factor(helpMeanAge):helpB+helpSonCat+helpUnrelateCat+
                      (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                    data=subset(offspring,helpNum<2), family="binomial")
summary(inde.helpFACa)


#removing all non-helper related effects
inde.helpFAC1<-glmer(independent~as.factor(helpMeanAge)+helpSonB+
                      (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                    data=subset(offspring,helpNum==1), family="binomial")
summary(inde.helpFAC1)
#model does converge
#negative effect of a 2 year old helper, no effect of older helpers

#removing the 1 year old helpers
inde.helpFAC2<-glmer(independent~as.factor(helpMeanAge)+helpSonB+
                       (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                     data=subset(offspring,helpNum==1& helpMeanAge>1), family="binomial")
summary(inde.helpFAC2)
#there are not any significant positive effects of older helpers 
#although all coefficients are trending NS positive with the exception of helper age 6


####################
#recruitment


#helper category
recruitHA3<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA3)
#effect of help age is the same
#negative effect of having 1 unrelated helper

#helper number
recruitHA4<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonNum+helpUnrelateNum+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA4)
#effect of help is reduced (p=0.04)
#no effects of the number of unrelated or related helpers

#excluding 1 year old helpers
#helper category


table(offspring$helpUnrelateNum)
table(offspring$helpSonNum)
1249/1618
#77%of territories with unrelated helpers had only one
#94% of territories either had none, or one unrelated helper (with the remaining 6% having 2-4)




