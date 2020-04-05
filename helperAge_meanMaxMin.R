#effects of helper mean, max and min ages

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")

###########################################
#weight

#mean helper age
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMeanAge:helpB+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=offspring,control=lmerControl(optimizer ="bobyqa")))

#mean helper age
#excluding WP 
summary(weight.haEP<-lmer(weight~Mage+Bage+Sage+lifespanB+lifespanS+
                          lifespanM+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMeanAge:helpB+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=subset(offspring,EPb=1),control=lmerControl(optimizer ="bobyqa")))

#max helper age
summary(weight.ha1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMaxAge:helpB+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=offspring,control=lmerControl(optimizer ="bobyqa")))

#min helper age
summary(weight.ha1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                           lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMinAge:helpB+
                           (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                         data=offspring,control=lmerControl(optimizer ="bobyqa")))
#no significant age effects in any model
#removing RE of dad_soc does not change model, although res var is slightly higher


############################################
#survival to independence

#mean helper age
inde.ha4<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMeanAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.ha4)
#Mage p = 0.06
#help p =0.03

#mean helper age
inde.hat<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpB/helpNum+helpMeanAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.hat)
#when helpers is included as a binary, helper mean age is NS



#mean helper age
#removing distinction between bio Sage and social sage
#i.e. only including EP chicks
inde.ha41<-glmer(independent~Mage+Bage+Sage+lifespanB+lifespanS+lifespanM+Jincube+helpNum+helpMeanAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=subset(offspring,EPb=1), family="binomial")
summary(inde.ha41)
#mage p= 0.06
#help age =  0.05

inde.ha41a<-glmer(independent~Mage+Bage:EPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+EPb+lifespanM+Jincube+helpNum+helpMeanAge:helpB+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=subset(offspring), family="binomial")
summary(inde.ha41a)
#Mage p= 0.13
#help age p =0.02

#max helper age
inde.ha4max<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpNum+helpMaxAge:helpB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.ha4max)
#sig effect of max help age

#min helper age
inde.ha4min<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                     EPb+Jincube+helpNum+helpMinAge:helpB+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                   data=offspring, family="binomial")
summary(inde.ha4min)
#sig effect of min help age

#simplifying by removing other age terms
#effect of min and max helper age for chicks with more than 1 helper
inde.ha4c<-glmer(independent~Jincube+helpNum+helpMinAge+helpMaxAge+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=subset(offspring,helpNum>1), family="binomial")
summary(inde.ha4c)
#no effect of either min or max age

#min and max helper ages in the same model
#exclude those with one helper
offspring <-offspring %>%
  mutate(help2Plus=ifelse(helpNum>1,1,0))
inde.ha4d<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpNum+helpMinAge:help2Plus+helpMaxAge:help2Plus+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.ha4d)
#neither are significant

#######################################################################################
#recruitment

male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")

#mean helper age
recruitHAa<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpNum+helpMeanAge:helpB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAa)


#mean helper age
#removing distinction between bio Sage and social sage
#i.e. only including EP chicks
recruitHAa1<-glmer(recruit~Mage+Bage+Sage+lifespanB+lifespanS+lifespanM+Jincube+helpNum+helpMeanAge:helpB+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(male.recruit,EPb=1), family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAa1)

#max helper age
recruitHAmax<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpNum+helpMaxAge:helpB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAmax)
#positive effect of max age approaches significance (effect: 0.11,p=0.08)

#min helper age
recruitHAmin<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                      EPb+Jincube+helpNum+helpMinAge:helpB+
                      (1|cohort)+(1|mum)+(1|dad_bio),
                    data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAmin)
#positive effect of min age is significant (effect: 0.14, p=0.03)

#min and max helper ages in the same model
#exclude those with one helper
male.recruit <-male.recruit %>%
  mutate(help2Plus=ifelse(helpNum>1,1,0))


recruitHAmin1<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                      EPb+Jincube+helpNum+helpMinAge:help2Plus+helpMaxAge:help2Plus+
                      (1|cohort)+(1|mum)+(1|dad_bio),
                    data=subset(male.recruit), family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAmin1)
#neither is significant
#min age is NS in the positive direction
#max is highly NS (negative direction)



