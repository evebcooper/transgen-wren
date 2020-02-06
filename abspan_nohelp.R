###########
#running analysis of all traits excluding helper variable



#survival to inde
inde.sd<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),,control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.sd)
#positive effect of WP dad
#positive effect of maternal lifespan
#results are not different from results w/out helpers


#weight)
weight.nh<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.nh)
#WP Bage now significant (not with helpers)


#weight sex interactions
weight.sex1<-lmer(weight~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                   lifespanM+EPb+Jincube+weight.age+pre1992+sex+No.hatched+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring)
summary(weight.sex1)
#interaction with Mage

#male recruitment
male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespan>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")

recruit<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruit)

############
#lifespan
offspring_long<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex!="U")%>%
  filter(lifespan>0)%>%
  filter(sire!="WG")

var(wf$lifespan,na.rm=TRUE)
var(wm$lifespan,na.rm=TRUE)

#remove females that never built a nest
#may have lived to age 1 but never left natal territory
fem_long<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex=="F") 
fem_longT<-semi_join(fem_long,nestRAW, by=c("ID"="Mother.ID"))
glmmTMBControl(optCtrl=list(iter.max=1e8,eval.max=1e8))
life.f1a<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 family=nbinom2(),data=fem_longT)
summary(life.f1a)
#female model is difficult to fit due to small sample size
#no effects of interest

#lifespan - males
life.m1<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.m1)
#positive effect of WP father lifespan
#same as with help


##################################
#LRS
offspring_lrs<-offspring %>%
  filter(fate=="Died") %>%
  filter(lifespan>0)%>%
  filter(independent==1)


#males - nbinom1
lrs.m1a<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                ziformula = ~.,
                data=subset(offspring_lrs,sex=="M"),family=nbinom1)
summary(lrs.m1a)
#no sig effects


#females - removed non-recruited
lrs.f2b<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+
                   (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                 ziformula = ~.,
                 data=fem_longT,family=nbinom1)
summary(lrs.f2b)
#model convergence issues
#highly significant positive effet of WP father lifespan on probability of LRS
#positive effect of WP father age on probability of LRS
#positive effect of EP bio fathre lifespan on LRS number
#positive effect of maternal lifespan on LRS number


#females - removed non-recruited
#removing EP fathers
lrs.fT<-glmmTMB(LRStot~Mage+Bage:WPb+lifespanB:WPb+lifespanM+
                   WPb+
                   (1|mum)+(1|dad_bio)+(1|cohort) ,
                 ziformula = ~.,
                 data=fem_longT,family=nbinom1)
summary(lrs.fT)
#still convergence issues
# still positive effect of WP father lifespan
#other effects dissapeared

#testing all fathers (EP and WP) together
lrs.fT1<-glmmTMB(LRStot~Mage+Bage+lifespanB+lifespanM+
                  (1|mum)+(1|dad_bio)+(1|cohort) ,
                ziformula = ~.,
                data=fem_longT,family=nbinom1)
summary(lrs.fT1)
#serious convergence issues

###############################################################################
#depreciated
#these models include females that lived to age one but may not have recruited

#females LRS
#this includes females that lived to age one but may not have recruited
glmmTMBControl(optCtrl=list(iter.max=1e4,eval.max=1e4))
lrs.f2a<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+
                   (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                 ziformula = ~.,
                 data=subset(offspring_lrs,sex=="F"),family=nbinom1)
summary(lrs.f2a)
#nothing sig


#LRS both sexes
#this includes females that lived to one year but did not recruit
lrs.all1<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+
                    (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                  ziformula = ~.,
                  data=offspring_lrs,family=nbinom1)
summary(lrs.all1)
#nothing significant (same as with helpers)

#############
#lifespan - base
life1<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               family=nbinom2(),data=offspring_long)
summary(life1)
#borderline positive effect (p=0.06) of WP lifespan
#same as without help

#lifespan - sex interactions
life.sex1<-glmmTMB(lifespan~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                     EPb+Jincube+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   family=nbinom2(),data=offspring_long)
summary(life.sex1)
#no interactions

#lifespan - females
life.f1<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 family=nbinom2(),data=subset(offspring_long,sex=="F"))
summary(life.f1)
#nothing significant
#same as with help


