#Running analysis with absolute age and lifespan

#################
#inde
wf<-filter(offspring,sex=="F")
wm<-filter(offspring,sex=="M")
var(wf$independent,na.rm=TRUE)
var(wm$independent,na.rm=TRUE)
#variances are pretty similar so no extra modelling needed
mean(wf$independent,na.rm=TRUE)
mean(wm$independent,na.rm=TRUE)
#means are very similar too


#survival to inde
inde.sd<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),,control=glmerControl(optimizer ="bobyqa"),
                  data=offspring, family="binomial")
summary(inde.sd)
#positive effect of WP Bage
#borderline (p=0.06) negative effect of maternal age
#positive effect of maternal lifespan

#survival-including brood size
inde.sd1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+No.hatched+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),,control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.sd1)
#didn't change results and NS

#survival:females
inde.f<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),,control=glmerControl(optimizer ="bobyqa"),
               data=subset(offspring,sex=="F"), family="binomial")
summary(inde.f)
#nothing sig anymore (going in correct directions)

#survival:males
inde.m<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                EPb+Jincube+help.cat+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),,control=glmerControl(optimizer ="bobyqa"),
              data=subset(offspring,sex=="M"), family="binomial")
summary(inde.m)
#maternal lifespan still improves survival (p=0.006 - same as full model)
#nothing else sig anymore

#survival:sex int.
inde.sex<-glmer(independent~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.sex)
#no interactions

#######################
#weight
#brood size removed because NS (although it does not change results)
weight.sd<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+help.cat+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.sd)
#nothing significant



#weight - females
weight.f<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+I(Jincube^2)+weight.age+pre1992+No.hatched+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=lmerControl(optimizer ="bobyqa"),
                data=subset(offspring,sex=="F"))
summary(weight.f)
#nothing sig (mat lifespan p=0.09, WP Bage p=0.29)

#weight - males
weight.m<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                 lifespanM+EPb+Jincube+I(Jincube^2)+weight.age+pre1992+No.hatched+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=lmerControl(optimizer ="bobyqa"),
               data=subset(offspring,sex=="M"))
summary(weight.m)
#positive effect of maternal lifespan 
#WP Bage positive effect becomes NS (p=0.07)

#weight sex interactions
weight.sex<-lmer(weight~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+help.cat+weight.age+pre1992+sex+No.hatched+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring)
summary(weight.sex)
#interaction with mage

wf<-filter(offspring,sex=="F")
wm<-filter(offspring,sex=="M")
var(wf$weight,na.rm=TRUE)
var(wm$weight,na.rm=TRUE)
#variances are pretty similar so no extra modelling needed
mean(wf$weight,na.rm=TRUE)
mean(wm$weight,na.rm=TRUE)
#means are very similar too

#############################
#male recruitment
male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespan>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")

recruitH<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+Jincube+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
ss <- getME(recruitH,c("theta","fixef"))
recruitH<- update(recruitH,start=ss,control=glmerControl(optCtrl=list(maxfun=10e6)))
summary(recruitH)

##########################
#lifespan
offspring_long<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex!="U")%>%
  filter(lifespan>0)%>%
  filter(sire!="WG")

var(wf$lifespan,na.rm=TRUE)
var(wm$lifespan,na.rm=TRUE)

#lifespan - base
life<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                EPb+Jincube+help.cat+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life)
#borderline positive effect (p=0.07) of WP lifespan

#lifespan - sex interactions
life.sex<-glmmTMB(lifespan~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                EPb+Jincube+help.cat+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life.sex)
#no interactions

#lifespan - females
life.f<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                EPb+help.cat+Jincube+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=subset(offspring_long,sex=="F"))
summary(life.f)
#nothing significant
#overdispersion 75.5



#lifespan - males
life.m<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+help.cat+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.m)
#positive effect of WP father lifespan
#NB there seems to be additive genetic and environmental quality since both EP Bage and EP Sage are pos NS

offspringf<-filter(offspring_long,sex=="F")
offspringm<-filter(offspring_long,sex=="M")
hist(offspringf$lifespan)
table(offspring_long$sex,offspring_long$lifespan)
var(offspringf$lifespan,na.rm=TRUE)
mean(offspringf$lifespan,na.rm=TRUE)
var(offspringm$lifespan,na.rm=TRUE)

##################################
#LRS
offspring_lrs<-offspring %>%
  filter(fate=="Died") %>%
  filter(lifespan>0)

#both sexes
lrs.all<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                 (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
               ziformula = ~.,
               data=offspring_lrs,family=nbinom1)
summary(lrs.all)
#nothing significant

#males
lrs.m<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                  (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                ziformula = ~.,
                data=subset(offspring_lrs,sex=="M"),family=nbinom1)
summary(lrs.m)
#borderline (p=0.052) negative effect of EP bio dad lifespan on probability of LRS
#borderline (p=0.06) negative effect of WP Bage on LRS number

#males - adding brood size
lrs.m1<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+No.hatched+
                 (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
               ziformula = ~.,
               data=subset(offspring_lrs,sex=="M"),family=nbinom1)
summary(lrs.m1)
#brood size NS 
#made borderline effects NS


#females
#nbinom2 didn't fit so went with nbinom1
glmmTMBControl(optCtrl=list(iter.max=1e4,eval.max=1e4))
lrs.f2<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                 (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
               ziformula = ~.,
               data=subset(offspring_lrs,sex=="F"),family=nbinom1)
summary(lrs.f2)
#nothing sig

