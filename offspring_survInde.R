library(sjPlot)
tab_model(inde.all,transform="plogis")
tab_model(inde.alla,inde.all,indeEP,indeWP,transform=NULL,show.icc=FALSE,title = "Survival to Independence",
          dv.labels =c( "All offspring","All offspring - interaction", "Extra-pair offspring","Within-pair offspring"))

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
####
#correlation between effects
#####
library(mctest)
detach("package:ppcor", unload=TRUE)
detach("package:MASS", unload=TRUE)
multi<-offspring%>%
  select(Mage,Bage,Sage,EP)
multi<-drop_na(multi)
multi<-data.matrix(multi)

library(ppcor)
pcor(multi, method = "spearman")

indeV<-(offspring$independent)
omcdiag(multi,indeV)

imcdiag(multi,indeV)



################################
#survival to independence
###############################

inde<-glmer(independent~MageREL+BageREL+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring, family="binomial")
summary(inde)

#########
#WP and EP offspring
############

#base model
inde.all<-glmer(independent~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring, family="binomial")
ss <- getME(inde.all,c("theta","fixef"))
inde.all<- update(inde.all,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all)
#highly significant interaction with EP
#EP Bage have a worse effect on offspring fitness

#base model - no Jincube
inde.all1<-glmer(independent~Mage+Bage*EP+Sage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring, family="binomial")
ss <- getME(inde.all1,c("theta","fixef"))
inde.all1<- update(inde.all1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all1)
#effects are roughly the same so we can keep this model for simplicity

inde.rg <- ref_grid(inde.all1)
inde.em<-emmeans(inde.rg,specs="Bage",by="EP")
test(inde.em)
#both are significantly negative, EP Bage is more negative

#Sage*EP interaction
inde.all2<-glmer(independent~Mage+Sage*EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.all2,c("theta","fixef"))
inde.all2<- update(inde.all2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all2)
#significant interaction

#Mage*EP interaction
inde.allM<-glmer(independent~Mage*EP+Sage+Bage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
summary(inde.allM)
#significant interaction, mothers invest less in EP offspring at they get older
#(NB this might be because they chose older EP males as they age?)

#Mage*EP interaction and Bage*EP
inde.allM1<-glmer(independent~Mage*EP+Sage+Bage*EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.allM1,c("theta","fixef"))
inde.allM1<- update(inde.allM1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.allM1)
#when Bage*EP interaction included, Mage*EP interaction loses significance (p=0.07)

#Mage*EP interaction and Sage*EP
inde.allM2<-glmer(independent~Mage*EP+Sage*EP+Bage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=offspring, family="binomial")
ss <- getME(inde.allM2,c("theta","fixef"))
inde.allM2<- update(inde.allM2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.allM2)
#Mage*EP is no longer significant (p=0.07)

#EP as fixed effect
inde.all4<-glmer(independent~Mage+Bage+Sage+EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
summary(inde.all4)

#EP as fixed effect - rel and mean
inde.all5<-glmer(independent~MageM+BageM+SageM+MageREL+BageREL+SageREL+EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.all5,c("theta","fixef"))
inde.all5<- update(inde.all5,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all5)

#EP interaction with Mage - rel and mean
inde.all8<-glmer(independent~MageM*EP+BageM*EP+MageREL*EP+BageREL*EP+EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.all8,c("theta","fixef"))
inde.all8<- update(inde.all8,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all8)

#EP interaction - rel and mean
inde.all6<-glmer(independent~MageM+BageM*EP+MageREL+BageREL*EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.all6,c("theta","fixef"))
inde.all6<- update(inde.all6,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all6)
#when you split them up the interaction remains in both, slightly stronger for relative age (p=0.055 for mean age)

#EP interaction - only rel, fixed mean
inde.all7<-glmer(independent~MageM+BageM+MageREL+BageREL*EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
summary(inde.all7)
#here the interaction is only borderline significant (p=0.04)
#this suggests the difference between EP and WP response is both due to:
#1.within indiv changes (fathers get better at parental care as they age and provide more to their offspring)
#2.between indiv changes (fathers that live longer are more capable for caring for their offspring)

#Sage*EP*Bage interaction
inde.all3<-glm(independent~Bage*EP+Sage*EP,
                 data=offspring, family="binomial")
ss <- getME(inde.all2,c("theta","fixef"))
inde.all2<- update(inde.all2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all3)
#cannot do a 3 way interaction (rank deficient)

#EP only
indeEP1<-glmer(independent~Mage+Bage+Sage+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeEP1,c("theta","fixef"))
indeEP1<- update(indeEP1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEP1)
#effect of Bage is negative but NS (and effect of Sage is the same)
#nothing changes from model without Jincube

#EP only - rel and mean
indeEP1a<-glmer(independent~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeEP1a,c("theta","fixef"))
indeEP1a<- update(indeEP1a,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEP1a)
#for Sage, the neg. effect is predominately driven by mean age
#for Bage, the negative effect is predominately driven by relative age

########
#WP only
######

indeWP<-glmer(independent~Mage+Bage+(1|cohort)+(1|mum)+(1|dad_bio),
              data=subset(offspring,EP=="no"), family="binomial")
summary(indeWP)
#NS positive effect of Bage

#WP only rel vs m
indeWPa<-glmer(independent~MageREL+MageM+BageREL+BageM+(1|cohort)+(1|mum)+(1|dad_bio),
              data=subset(offspring,EP=="no"), family="binomial")
ss <- getME(indeWPa,c("theta","fixef"))
indeWPa<- update(indeWPa,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeWPa)
#the positive effect of Bage/Sage within-pair is entirely driven by relative age

#WP only rel vs m quad
indeWPa<-glmer(independent~MageREL+MageM+BageREL+BageM+I(BageREL^2)+I(BageM^2)+(1|cohort)+(1|mum)+(1|dad_bio),
               data=subset(offspring,EP=="no"), family="binomial")
ss <- getME(indeWPa,c("theta","fixef"))
indeWPa<- update(indeWPa,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeWPa)

ggplot(subset(offspring,(EP=="yes")),aes(Bage,independent))+
  geom_smooth()+
  scale_y_continuous(limits=c(0,1))

#Males: WP only rel vs m
indeWPam<-glmer(independent~MageREL+MageM+BageREL+BageM+(1|cohort)+(1|mum)+(1|dad_bio),
               data=subset(offspring,EP=="no"&sex=="M"), family="binomial")
summary(indeWPam)
#positive effect of mean maternal age

#Females: WP only rel vs m
indeWPaf<-glmer(independent~MageREL+MageM+BageREL+BageM+(1|cohort)+(1|mum)+(1|dad_bio),
                data=subset(offspring,EP=="no"&sex=="F"), family="binomial")
summary(indeWPaf)
#positive effect of mage m

##################################
#all offspring: relative age
inde.allR<-glmer(independent~MageREL+BageREL*EP+BageM+MageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
summary(inde.allR)
#significant effect 


#EP only relative age
indeEPr<-glmer(independent~MageREL+BageREL+SageREL+SageM+MageM+BageM+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeEPr,c("theta","fixef"))
indeEPr<- update(indeEPr,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEPr)
#mage rel is negative
#effect of Bage is negative but NS (and effect of Sage is the same)


##################
#all offspring: relative and mean age
inde.allR1<-glmer(independent~MageREL+BageREL*EP+MageM+BageM*EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
ss <- getME(inde.allR1,c("theta","fixef"))
inde.allR1<- update(inde.allR1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.allR1)
#significant interaction with relative age, boderline significant interaction with mean age


#EP only relative age
indeEPr1<-glmer(independent~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeEPr1,c("theta","fixef"))
indeEPr1<- update(indeEPr1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEPr1)
#effect of Bage  and Sage (rel and mean) are all negative NS


#EP only relative age - sex effects
indeEPr1s<-glmer(independent~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeEPr1s,c("theta","fixef"))
indeEPr1s<- update(indeEPr1s,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEPr1s)
#does not converge

#EP only relative age - females
indeEPr1f<-glmer(independent~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,EP=="yes"&sex=="F"), family="binomial")
summary(indeEPr1f)
#no effects

#EP only relative age - males
indeEPr1m<-glmer(independent~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,EP=="yes"&sex=="M"), family="binomial")
ss <- getME(indeEPr1m,c("theta","fixef"))
indeEPr1m<- update(indeEPr1m,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEPr1m)
#effect of Bage  and Sage (rel and mean) are all negative NS


#############################
#males only
inde.m<-glmer(independent~Mage+Bage*EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sex=="M"), family="binomial")
summary(inde.m)
#significant neg interaction


#EP males only
indeEPm<-glmer(independent~Mage+Bage+Sage+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=subset(offspring,EP=="yes"&sex=="M"), family="binomial")
summary(indeEPm)
#negative NS effect of both Bage and Sage

#WP males only
indeWPm<-glmer(independent~Mage+Bage+(1|cohort)+(1|mum)+(1|dad_bio),
              data=subset(offspring,EP=="no"&sex=="M"), family="binomial")
summary(indeWPm)
#NS positive effect of Bage

#males only
inde.m<-glmer(independent~Mage+Bage*EP+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=subset(offspring,sex=="M"), family="binomial")
summary(inde.m)
#significant neg interaction


#EP females only
indeEPf<-glmer(independent~Mage+Bage+Sage+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=subset(offspring,EP=="yes"&sex=="F"), family="binomial")
summary(indeEPf)
#NS effect of both Bage and Sage

#WP females only
indeWPf<-glmer(independent~Mage+Bage+(1|cohort)+(1|mum)+(1|dad_bio),
               data=subset(offspring,EP=="no"&sex=="F"), family="binomial")
summary(indeWPf)
#NS positive effect of Bage



