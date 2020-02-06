library(dplyr)
library(glmmTMB)
library(sjPlot)

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
offspring_lrs<- offspring %>%
  filter(fate=="Died")
offspringL<-offspring %>%
  filter(!is.na(lifespan))

########
#survival to independence
#######

#base model
inde.ls<-glmer(independent~EP+Jincube+Mage*lifespanM+Bage*lifespanB+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring, family="binomial")
ss <- getME(inde.ls,c("theta","fixef"))
inde.ls<- update(inde.ls,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.ls)
#convergence issues still
#no interactions

#base model - simplified
inde.lsA<-glmer(independent~Bage*lifespanB+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=offspring, family="binomial")
ss <- getME(inde.lsA,c("theta","fixef"))
inde.lsA<- update(inde.lsA,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.lsA)
#model converges and borderline sig effect (p=0.06) but may be due to variance attributable to other sources

#EP only
inde.ls1<-glmer(independent~Jincube+Mage*lifespanM+Bage*lifespanB+Sage*lifespanS+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=subset(offspring,EP="yes"), family="binomial")
ss <- getME(inde.ls,c("theta","fixef"))
inde.ls<- update(inde.ls,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.ls1)

#######
#offspring weight - 
######

weight.ls<-lmer(weight~Mage*lifespanM+Bage*lifespanB+EP+Jincube+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring)
summary(weight.ls)


#######
#LRS 
#######

LRS.ls<-glmmTMB(LRStot~Mage*lifespanM+Bage*lifespanB+EP+Jincube+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=offspring_lrs,family=truncated_nbinom1)
summary(LRS.ls)



#######
#lifespan
######

long.ls<-glmmTMB(lifespan~Mage*lifespanM+Bage*lifespanB+EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~.,
                  data=offspringL,family=truncated_nbinom2)
summary(long.ls)
#positive interaction between Bage and male sex in probability to surviving to 1 approaches sig (p=0.09)
#suggests the negative effect of Bage is worse for male offspring

#######
#table results
########

tab_model(weight.sex,transform=c(NULL),show.icc=FALSE,title = "Effect of Sex",
          dv.labels =c( "Weight"))

tab_model(inde.sex,transform=c(NULL),show.icc=FALSE,title = "Effect of Sex",
          dv.labels =c( "Survival to Independence"))

tab_model(long.sex,transform=c(NULL),show.icc=FALSE,title = "Effect of Sex",
          dv.labels =c( "Lifespan"))

tab_model(LRS.sex,transform=c(NULL),show.icc=FALSE,title = "Effect of Sex",
          dv.labels =c( "LRS"))






