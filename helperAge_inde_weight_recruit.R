#including helper ages

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")



#################
#survival to independence
#original model shows positive effect of mat lifespan,
#borderline positive effect of one helper (but no effect of 2 plus helpers)
#positive effect of Bage WPb

#helper category, mean age
inde.ha<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+help.cat+helpMeanAge:helpB+
                  (1|mum)(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.ha)


#change random effect to nest ID (mean age)
inde.ha2<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+help.cat+helpMeanAge:helpB+
                  (1|mum/Nest.ID.mother),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.ha2)
#model does converge
#maternal lifespan and Mage significant, but nothing else is

#random effects of nest ID, and parent IDs and cohort
inde.ha2a<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMeanAge:helpB+
                  (1|mum/Nest.ID.mother)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.ha2a)
#convergence issues

#random effects of nest ID, and mum and biodad and cohort
inde.ha2b<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpNum+helpMeanAge:helpB+
                   (1|mum/Nest.ID.mother)+(1|cohort)+(1|dad_bio),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.ha2b)
#still convergence issues

#helper number rather than category
inde.ha4<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMeanAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.ha4)
#previously sig effects remain (maternal lifespan and Bage WP)
#mean helper age significantly improves survival!

#helper number rather than category
#helper max age
inde.ha5<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMaxAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.ha5)
#Bage WP is now borderline NS
#max helper age is signficant!

#only territories with helpers
#helper max age
inde.ha6<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMaxAge+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=subset(offspring,helpB=1), family="binomial")
summary(inde.ha6)
#Mage approaches sig, lifespan mat still significant
#help max age is significant, number of helpers is not
#Bage is not quite significant

#only territories with helpers
#helper mean age
inde.ha8<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMeanAge+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=subset(offspring,helpB=1), family="binomial")
summary(inde.ha8)
#same as above except that Bage WP is now significant

#only territories without helpers
inde.ha7<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=subset(offspring,helpB=0), family="binomial")
summary(inde.ha7)
#here BageWP is signficant, same with maternal lifespan




#######################################
#weight
#exclude helper lifespan
#helper age is typically not constrained by lifespan, but rather extrinsic conditions (becoming Dom), and so controlling is irrelevant

#mean helper age
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+help.cat+helpMeanAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa")))
#no sig effects of helpers or helper ages
#Bage has no effect (same as other model that controlled for helper number)

#trying random nest effect
weight.ha1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+help.cat+helpMeanAge:helpB+
                  (1|cohort)+(1|mum/Nest.ID.mother),
                data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.ha1)
#same as RE structure above


#helper max age
weight.ha2<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+help.cat+helpMaxAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.ha2)


#helper mean age
#remove helper categories, replace with number helpers
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMeanAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa")))




#max helper age, helper number
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum
                        +helpMaxAge:helpB+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=offspring,control=lmerControl(optimizer ="bobyqa")))

#only territories with helpers (max age)
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+helpMaxAge+helpNum+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=subset(offspring,helpNum>0),control=lmerControl(optimizer ="bobyqa")))
#when helpers are present, it's better to have an older Sage (but not older Bage WP)

#only territories with helpers (mean age)
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+helpMaxAge+helpNum+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=subset(offspring,helpNum>0),control=lmerControl(optimizer ="bobyqa")))
#same as for max helper age

#only territories without helpers
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=subset(offspring,helpNum=0),control=lmerControl(optimizer ="bobyqa")))
#when helpers are not present, it's better to have an older Bage WP (but not older Sage)


#################################################################################################################################
#male survival from banding to recruitment
#includes only males because female fate during this time is uncertain
#lifespan >0 indicates they lived until their first breeding season
male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")

#helper mean age
#helper number
recruitHA<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMeanAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
ss <- getME(recruitHA,c("theta","fixef"))
recruitHA<- update(recruitHA,start=ss,control=glmerControl(optCtrl=list(maxfun=10e6)))
summary(recruitHA)
#model fails to converge
#positive effect of mat lifespan and negative effect of maternal age remain
#positive value of helper age

#removing RE of dad_soc to attempt to improve model fit
recruitHAa<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpNum+helpMeanAge:helpB+
                   (1|cohort)+(1|mum)+(1|dad_bio),
                 data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAa)
#this now works, what you expect to be significant is

#helper category 
recruitHAa1<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+help.cat+helpMeanAge:helpB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAa1)


#measuring recruitment from independence
#rather than banding
male.recruitInde<- offspring %>%
  filter(sex=="M") %>%
  filter(independent==1) %>%
  #filter(fate!="Dispersedoutside")%>%
  #filter(fate!="Unknown") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))

#helper mean age
#helper number
recruitHA1<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpNum+helpMeanAge:helpB+
                   (1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                 data=male.recruitInde, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA1)
#singular fit
#nothing significant

#removing random effect of dad_soc to attempt to improve fit
recruitHA1a<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpNum+helpMeanAge:helpB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruitInde, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA1a)
#still singular fit, still nothing significant
