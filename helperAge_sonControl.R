#graph mean son helper again inde and recruit
offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")

#include number of son and unrelated helpers as seperate variables
inde.son<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+helpMeanAge:helpB+helpUnrelateNum+helpSonNum+
                 (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.son)
#results are the same, effect of helper age still significant
#neither number of unrelated or related helpers are significant

#helpMeanAge is only estimated for those that have son helpers
#help number is controlled for
inde.son1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpMeanAge:helpSonB+helpNum+
                  (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.son1)
#effect is very NS

#helpMeanAge is only estimated for those that have UNRELATED helpers
#help number is controlled for
offspring<-mutate(offspring, helpUnrelateB=ifelse(helpUnrelateNum>0,1,0))


inde.son2<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpUnrelateB+helpNum+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son2)
#effect is also NS
#this suggests it's likely a power issue


#mean son helper age effect
#control for both number unrelated and sons
inde.son3<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpSonMean:helpSonB+helpUnrelateNum+helpSonNum+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son3)
#no effect of age
#weirdly there is a negative effect of having more helper sons
#note that now Mage and lifespanM are not important


#mean age effect of all helpers
#control for binary having son helpers, and binary having unrelated helpers
inde.son4<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                  (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                data=offspring, family="binomial")
summary(inde.son4)
#highly significant effect of helper age!
#also negative effect of having an unrelated helper

#is there an interaction between having a helper son and the help mean age effect?
inde.son4a<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB*helpSonB+helpUnrelateB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son4a)
#nope


###########
#weight
summary(weight.ha1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+
                           helpMeanAge:helpB+helpSonB+helpUnrelateB+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=offspring,control=lmerControl(optimizer ="bobyqa")))
#no significant effects

#recruitment
recruitHA1<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA1)
#significant effect of helper age


#recruitment - excluding those that didnt reach inde
male.recruitInde<- offspring %>%
  filter(sex=="M") %>%
  filter(independent==1) %>%
  #filter(fate!="Dispersedoutside")%>%
  #filter(fate!="Unknown") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))
recruitHAinde<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruitInde, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAinde)
#model does not converge




