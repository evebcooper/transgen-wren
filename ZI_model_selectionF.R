offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
library(glmmTMB)
library(bbmle)
library(dplyr)


#overall LRS
offspring_lrstot<- offspring %>%
  filter(fate=="Died") 

NB2lrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+MageM+BageM+ 
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=nbinom2)
NB1lrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=nbinom1)
#compoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                     (1|cohort)+(1|mum)+(1|dad_bio) ,
#                  ziformula = ~.,
#                 data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=compois)
genpoislrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                      (1|cohort)+(1|mum)+(1|dad_bio) ,
                    ziformula = ~.,
                    data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=genpois(link="log"))
tNB2lrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=truncated_nbinom2)
tNB1lrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=truncated_nbinom1)
#tcompoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                      (1|cohort)+(1|mum)+(1|dad_bio) ,
#                   ziformula = ~.,
#                  data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=truncated_compois)
tgenpoislrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                       (1|cohort)+(1|mum)+(1|dad_bio) ,
                     ziformula = ~.,
                     data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=truncated_genpois(link="log"))
tpoislrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=truncated_poisson(link="log"))
tweedielrsf<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                      (1|cohort)+(1|mum)+(1|dad_bio) ,
                    ziformula = ~.,
                    data=subset(offspring_lrstot,EP=="yes"&sex=="F"),family=tweedie(link="log"))
AICtab(NB2lrsf,NB1lrsf,genpoislrsf,tNB2lrsf,tNB1lrsf,tgenpoislrsf,tpoislrsf,tweedielrsf)
#when including Sage
summary(NB2lrsf)

#when excluding Sage variables
#tgenpoislrsf is best
#genpoislrsf is >0.3
summary(tgenpoislrsf)
summary(genpoislrsf)


#lifespan

offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)

NB2lf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=nbinom2)
NB1lf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=nbinom1)
#compoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                  (1|cohort)+(1|mum)+(1|dad_bio) ,
#                 ziformula = ~.,
#                  data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=compois)
genpoislf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=genpois(link="log"))
tNB2lf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_nbinom2)
tNB1lf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_nbinom1)
#tcompoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                    (1|cohort)+(1|mum)+(1|dad_bio) ,
#                 ziformula = ~.,
#                data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_compois)
tgenpoislf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                     (1|cohort)+(1|mum)+(1|dad_bio) ,
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_genpois(link="log"))
tpoislf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_poisson(link="log"))
tweedielf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=tweedie(link="log"))
AICtab(NB2lf,NB1lf,tNB2lf,tNB1lf,tgenpoislf,tpoislf,tweedielf)

#when dad_soc included
summary(tpoislf)

#best model when dad_soc excluded
summary(tNB2lf)

#fecundity rate
offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)

NB2f<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
               (1|cohort)+(1|mum)+(1|dad_bio) ,
             ziformula = ~.,
             data=subset(offspring_fec,EP=="yes"&sex=="F"),family=nbinom2)
NB1f<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
               (1|cohort)+(1|mum)+(1|dad_bio) ,
             ziformula = ~.,
             data=subset(offspring_fec,EP=="yes"&sex=="F"),family=nbinom1)
compoisf<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="F"),family=compois(link="log"))
genpoisf<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="F"),family=genpois(link="log"))
tNB2f<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_fec,EP=="yes"&sex=="F"),family=truncated_nbinom2)
tNB1f<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_fec,EP=="yes"&sex=="F"),family=truncated_nbinom1)
tcompoisf<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_fec,EP=="yes"&sex=="F"),family=truncated_compois)
tgenpoisf<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_fec,EP=="yes"&sex=="F"),family=truncated_genpois)
tpoisf<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_fec,EP=="yes"&sex=="F"),family=truncated_poisson)
tweedief<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="F"),family=tweedie(link="log"))
AICtab(NB2f,NB1f,genpoisf,tweedief,compoisf)

#when sage is included nothing fits

#compoisf is only model when Sage excluded
summary(compoisf)

