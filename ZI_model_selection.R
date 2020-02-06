offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
library(glmmTMB)
library(bbmle)
library(dplyr)


#overall LRS
offspring_lrstot<- offspring %>%
  filter(fate=="Died") 

table(offspring_lrstot$LRStot)

NB2lrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrstot,EP=="yes"),family=nbinom2)
NB1lrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrstot,EP=="yes"),family=nbinom1)
#compoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
 #                     (1|cohort)+(1|mum)+(1|dad_bio) ,
  #                  ziformula = ~.,
   #                 data=subset(offspring_lrstot,EP=="yes"),family=compois)
genpoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                      (1|cohort)+(1|mum)+(1|dad_bio) ,
                    ziformula = ~.,
                    data=subset(offspring_lrstot,EP=="yes"),family=genpois(link="log"))
tNB2lrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="yes"),family=truncated_nbinom2)
tNB1lrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="yes"),family=truncated_nbinom1)
#tcompoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
 #                      (1|cohort)+(1|mum)+(1|dad_bio) ,
  #                   ziformula = ~.,
   #                  data=subset(offspring_lrstot,EP=="yes"),family=truncated_compois)
tgenpoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                       (1|cohort)+(1|mum)+(1|dad_bio) ,
                     ziformula = ~.,
                     data=subset(offspring_lrstot,EP=="yes"),family=truncated_genpois(link="log"))
tpoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,EP=="yes"),family=truncated_poisson(link="log"))
tweedielrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                      (1|cohort)+(1|mum)+(1|dad_bio) ,
                    ziformula = ~.,
                    data=subset(offspring_lrstot,EP=="yes"),family=tweedie(link="log"))
AICtab(NB2lrs,NB1lrs,genpoislrs,tNB2lrs,tNB1lrs,tgenpoislrs,tpoislrs,tweedielrs)
#best is NB1lrs
summary(NB1lrs)

####when random effect of dad_soc is included:
#best is NB1LRS
#tgonpoislrs, tNB1lrs, NB2lrs, and tNB2lrs are all <3 dAIC from best


#lifespan

offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)

NB2l<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_lrs1,EP=="yes"),family=nbinom2)
NB1l<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
#compoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
  #                  (1|cohort)+(1|mum)+(1|dad_bio) ,
 #                 ziformula = ~.,
#                  data=subset(offspring_lrs1,EP=="yes"),family=compois)
genpoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrs1,EP=="yes"),family=genpois(link="log"))
tNB2l<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom2)
tNB1l<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
#tcompoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
 #                    (1|cohort)+(1|mum)+(1|dad_bio) ,
  #                 ziformula = ~.,
   #                data=subset(offspring_lrs1,EP=="yes"),family=truncated_compois)
tgenpoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                     (1|cohort)+(1|mum)+(1|dad_bio) ,
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"),family=truncated_genpois(link="log"))
tpoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="yes"),family=truncated_poisson(link="log"))
tweediel<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrs1,EP=="yes"),family=tweedie(link="log"))
AICtab(NB2l,NB1l,genpoisl,tNB2l,tgenpoisl,tpoisl)

############zero-inflated
#without random effect of dad_soc:
#tgenpoisl
summary(tgenpoisl)


#when random effect of dad_soc is included
#tNB2l is best fit
summary(tNB2l)

#fecundity rate
offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)%>%
  filter(fate=="Died")

table(offspring_fec$fec.rate)
hist(subset(offspring_fec$fec.rate,offspring_fec$sex=="F"))


NB2<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
               (1|cohort)+(1|mum)+(1|dad_bio) ,
             ziformula = ~.,
             data=subset(offspring_fec,EP=="yes"),family=nbinom2)
NB1<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
               (1|cohort)+(1|mum)+(1|dad_bio) ,
             ziformula = ~.,
             data=subset(offspring_fec,EP=="yes"),family=nbinom1)
compois<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"),family=compois(link="log"))
genpois<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"),family=genpois(link="log"))
tNB2<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_fec,EP=="yes"),family=truncated_nbinom2)
tNB1<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_fec,EP=="yes"),family=truncated_nbinom1)
tcompois<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
        ziformula = ~.,
              data=subset(offspring_fec,EP=="yes"),family=truncated_compois)
tgenpois<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_fec,EP=="yes"),family=truncated_genpois)
tpois<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_fec,EP=="yes"),family=truncated_poisson)
tweedie<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"),family=tweedie(link="log"))
AICtab(NB2,NB1,genpois,compois)
#compois is the only model where AIC can be calculated
summary(compois)

#when dad_soc included as random effect
#compois is the only model where AIC can be calculated

#conditional
#borderline sig (p=0.05) negative effect of relative Bage
#boderline sig (p=0.05) positive effect of mean Bage
#positive effect of relative Sage (having an older dominant male suggests you may take over soon)
#binary
#nothing


#########################################################
#fecundity rate
#offset used for lifespan
#fecundity rate
offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)

hist(offspring_fec$fec.rate+0.001)
table(offspring_fec$fec.rate+0.001)

NB2o<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
             ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=nbinom2)
NB1o<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,ziformula = ~.,
             data=subset(offspring_fec,EP=="yes"),family=nbinom1)
compoiso<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=compois(link="log"))
genpoiso<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=genpois(link="log"))
tNB2o<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_nbinom2)
tNB1o<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_nbinom1)
tcompoiso<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_compois)
tgenpoiso<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_genpois)
tpoiso<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_poisson)
tweedieo<-glmmTMB(LRStot/lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=tweedie(link="log"))
AICtab(NB2o,NB1o,genpoiso,compoiso,tNB1o)
#AICtab(NB2o,NB1o,genpoiso,compoiso,tNB2o,tNB1o,tcompoiso,tgenpoiso,tpoiso,tweedieo)

#only compoiso works
summary(compoiso)
#the effects are identical to when fecundity is included as a rate
