library(sjPlot)

################################
#survival to independence
###############################

#########
#WP and EP offspring
############

#base model
inde.all<-glmer(independent~Mage+Bage*EP+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring, family="binomial")
ss <- getME(inde.all,c("theta","fixef"))
inde.all<- update(inde.all,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.all)
tab_model(inde.all,transform="plogis")
tab_model(inde.alla,inde.all,indeEP,indeWP,transform=NULL,show.icc=FALSE,title = "Survival to Independence",
          dv.labels =c( "All offspring","All offspring - interaction", "Extra-pair offspring","Within-pair offspring"))

#base model - no interaction
inde.alla<-glmer(independent~Mage+Bage+EP+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring, family="binomial")
summary(inde.alla)

#EP only 
indeEP<-glmer(independent~Mage+Bage+Sage+lifespanM+lifespanB+lifespanS+Jincube+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeEP,c("theta","fixef"))
indeEP<- update(indeEP,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEP)

#WP only
indeWP<-glmer(independent~Mage+Bage+lifespanM+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
              data=subset(offspring,EP=="no"), family="binomial")
ss <- getME(indeWP,c("theta","fixef"))
indeWP<- update(indeWP,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeWP)

