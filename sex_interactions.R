library(dplyr)
library(glmmTMB)
library(sjPlot)

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
offspring_lrs<- offspring %>%
  filter(fate=="Died")
offspringL<-offspring %>%
  filter(!is.na(lifespan))

########
#survival to independence - effect of sex
#######

#####
#WP and EP
#####
#base model
inde.sex<-glmer(independent~Mage+Bage*sex+EP+Jincube+lifespanM+lifespanB+lifespanS+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,sex!="U"), family="binomial")
ss <- getME(inde.sex,c("theta","fixef"))
inde.sex<- update(inde.sex,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde.sex)
#convergence issues still
#negative effect of being EP


#######
#offspring weight - effect of sex
######

weight.sex<-lmer(weight~Mage+Bage*sex+EP+Jincube+lifespanM+lifespanB+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sex!="U"))
summary(weight.sex)
#males are heavier at banding
#no interaction between dad age and offspring sex

#######
#LRS - effect of sex
#######

LRS.sex<-glmmTMB(LRStot~Mage+Bage*sex+EP+Jincube+lifespanM+lifespanS+lifespanB+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_lrs,sex!="U"),family=truncated_nbinom1)
summary(LRS.sex)
#no interactions with sex


#######
#lifespan - effect of sex
######

long.sex<-glmmTMB(lifespan~Mage+Bage*sex+EP+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~.,
                  data=subset(offspringL,sex!="U"),family=truncated_nbinom2)
summary(long.sex)
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












