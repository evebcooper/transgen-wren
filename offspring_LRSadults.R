#offspirng LRS models but excluding individuals that didn't make it to independence

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
library(glmmTMB)
library(bbmle)
library(dplyr)

offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex!="U")

####
#model selection
######
LRS.pois<-glmmTMB(LRStot~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspring_lrs1,family=poisson)
LRS.nb1<-glmmTMB(LRStot~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                 ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                 data=offspring_lrs1,family=nbinom1)
LRS.nb2<-glmmTMB(LRStot~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                 ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                 data=offspring_lrs1,family=nbinom2)
LRS.poisT<-glmmTMB(LRStot~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   data=offspring_lrs1,family=truncated_poisson)
LRS.nb1T<-glmmTMB(LRStot~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspring_lrs1,family=truncated_nbinom1)
LRS.nb2T<-glmmTMB(LRStot~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspring_lrs1,family=truncated_nbinom2)
AICtab(LRS.pois,LRS.nb1,LRS.nb2,LRS.poisT,LRS.nb1T,LRS.nb2T)
summary(LRS.nb1T)


#####
#WP and EP - no lifespan control
####
LRS2<-glmmTMB(LRStot~Mage+Bage+Jincube+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             ziformula = ~.,
             data=offspring_lrs1,family=truncated_nbinom1)
summary(LRS2)
#Bage stays negative but is no longer significant
#suggests that older males could be better just given the fact that older = didn't die young


#####
#WP and EP -females
####

LRSfem<-glmmTMB(LRStot~Mage+Bage+Jincube+lifespanM+lifespanB+lifespanS+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             ziformula = ~.,
             data=subset(offspring_lrs1,sex=="F"),family=truncated_nbinom1)
summary(LRSfem)
#effect of Bage no longer significant (p=0.10)

#####
#WP and EP - males
####

LRSm<-glmmTMB(LRStot~Mage+Bage+lifespanM+lifespanB+lifespanS+
                  (1|cohort)+(1|mum),
                ziformula = ~.,
                data=subset(offspring_lrs1,sex=="M"),family=truncated_nbinom1)
summary(LRSm)
#Bage is negative but NS

#####
#WP only
#####
LRS.WP<-glmmTMB(LRStot~Mage+Bage+Jincube+lifespanB+lifespanM+(1|cohort)+(1|mum)+(1|dad_bio),
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="no"),family=truncated_nbinom1)
summary(LRS.WP)
#NS

#####
#EP only
#####
LRS.EP<-glmmTMB(LRStot~Mage+Bage+Jincube+lifespanB+lifespanM+(1|cohort)+(1|mum)+(1|dad_bio),
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
summary(LRS.EP)
#NS but negative


