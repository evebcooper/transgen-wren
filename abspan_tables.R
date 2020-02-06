library(sjPlot)


#independence - help
inde.h<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+weight.res+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.h)

tab_model(inde.h,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date","Helpers [one]","Helpers [two+]",
                        "Weight (Residual)","Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,9,12,10,13,11,14,4,5,6,7,8))

#independence - no help
inde.sd<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+weight.res+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.sd)

tab_model(inde.sd,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date","Weight (Residual)",
                        "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-Pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,7,10,8,11,9,12,4,5,6))

#################
#weight - help
weight.sd<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+help.cat+weight.age+pre1992+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring)
summary(weight.sd)

tab_model(weight.sd,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date","Helpers [one]","Helpers [two+]",'Age at Weighing',
                        'Pre-1992',"Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,10,13,11,14,12,15,4,5,6,7,8,9))


#weight - no help
weight.nh<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.nh)

tab_model(weight.nh,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date",'Age at Weighing',
                        'Pre-1992',"Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,8,11,9,12,10,13,4,5,6,7))

################
#male recruitment - help
recruitH<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+help.cat+
                  (1|cohort)+(1|mum/dad_soc)+(1|dad_bio),
                data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
ss <- getME(recruitH,c("theta","fixef"))
recruitH<- update(recruitH,start=ss,control=glmerControl(optCtrl=list(maxfun=10e6)))
summary(recruitH)

tab_model(recruitH,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date","Helpers [one]","Helpers [two+]",
                        "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,8,11,9,12,10,13,4,5,6,7))


#male recruitment - no help
recruit<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum/dad_soc)+(1|dad_bio),
               data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruit)

tab_model(recruit,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date",
                        "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,6,9,7,10,8,11,4,5))

##########
#Lifespan
offspring_long<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex!="U")%>%
  filter(lifespan>0)%>%
  filter(sire!="WG")

#males - no help
life.m1<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.m1)

tab_model(life.m1,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date",
                        "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,6,9,7,10,8,11,4,5))

#males - help
life.m<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+help.cat+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.m)

tab_model(life.m,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Helpers [one]","Helpers [two+]",
                        "Incubation Date","Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,8,11,9,12,10,13,4,5,6,7))


# females - no help
life.f1<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 family=nbinom2(),data=subset(offspring_long,sex=="F"))
summary(life.f1)

tab_model(life.f1,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date",
                        "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,6,9,7,10,8,11,4,5))

#females - help
life.f<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+help.cat+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=subset(offspring_long,sex=="F"))
summary(life.f)

tab_model(life.f,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Helpers [one]","Helpers [two+]",
                        "Incubation Date","Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,8,11,9,12,10,13,4,5,6,7))

#####################
#LRS
offspring_lrs<-offspring %>%
  filter(fate=="Died") %>%
  filter(lifespan>0)%>%
  filter(independent==1)%>%
  filter(sex!="U")%>%
  filter(sire!="WG")

#males - no help
lrs.m1a<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                 ziformula = ~.,
                 data=subset(offspring_lrs,sex=="M"),family=nbinom2)
summary(lrs.m1a)

tab_model(lrs.m1a,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date",
                        "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,6,9,7,10,8,11,4,5))

#males - with help
lrs.m<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                 (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
               ziformula = ~.,
               data=subset(offspring_lrs,sex=="M"),family=nbinom1)
summary(lrs.m)

tab_model(lrs.m,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date","Helpers [one]","Helpers [two+]",
                        "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,8,11,9,12,10,13,4,5,6,7))


#females -no help
glmmTMBControl(optCtrl=list(iter.max=1e4,eval.max=1e4))
lrs.f2a<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+
                   (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                 ziformula = ~.,
                 data=subset(offspring_lrs,sex=="F"),family=nbinom1)
summary(lrs.f2a)

tab_model(lrs.f2a,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date",
                        "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,6,9,7,10,8,11,4,5))

#females - with help
######################################
#cannot get model to converge...
glmmTMBControl(optCtrl=list(iter.max=1e4,eval.max=1e4))
lrs.fHa<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+help.cat+
                  (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                ziformula = ~.,
                data=subset(offspring_lrs,sex=="F"),family=nbinom1)
summary(lrs.fHa)

#females - with help
glmmTMBControl(optCtrl=list(iter.max=1e4,eval.max=1e4))
lrs.f2H<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+help.cat+
                  (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                ziformula = ~.,
                data=subset(offspring_lrs,sex=="F"),family=nbinom2)
summary(lrs.f2H)

tab_model(lrs.f2H,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Group [yes]","Incubation Date","Helpers [one]","Helpers [two+]",
                        "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Extra-pair Social Father Lifespan", "Within-pair Father Lifespan"
          ),order.terms = c(1,2,3,8,11,9,12,10,13,4,5,6,7))



#########################
#sample sizes

i.sample <- offspring %>%
  filter(!is.na(Mage))%>%
  filter(!is.na(Bage))%>%
  filter(!is.na(Sage))%>%
  filter(!is.na(lifespanM))%>%
  filter(!is.na(lifespanB))%>%
  filter(!is.na(lifespanS))%>%
  filter(!is.na(help.cat))%>%
  filter(!is.na(EPb))

table(i.sample$sex)  
  
#did all the study_born_tidy except for the final stages removing mother-son pairings and helper-mother pairings

removed <- study_born %>%
  filter(!is.na(Mage))%>%
  filter(!is.na(Bage))%>%
  filter(!is.na(Sage))%>%
  filter(!is.na(lifespanM))%>%
  filter(!is.na(lifespanB))%>%
  filter(!is.na(lifespanS))%>%
  filter(!is.na(help.cat))%>%
  filter(!is.na(EPb))

table(removed$sire)
165/(2676+2060+165)

table(removed$incest)
141/(141+4760)

