offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
library(glmmTMB)
library(bbmle)
library(dplyr)


#overall LRS
offspring_lrstot<- offspring %>%
  filter(fate=="Died") 

NB2lrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=nbinom2)
NB1lrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=nbinom1)
#compoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                     (1|cohort)+(1|mum)+(1|dad_bio) ,
#                  ziformula = ~.,
#                 data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=compois)
genpoislrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                       (1|cohort)+(1|mum)+(1|dad_bio) ,
                     ziformula = ~.,
                     data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=genpois(link="log"))
tNB2lrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=truncated_nbinom2)
tNB1lrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=truncated_nbinom1)
#tcompoislrs<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                      (1|cohort)+(1|mum)+(1|dad_bio) ,
#                   ziformula = ~.,
#                  data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=truncated_compois)
tgenpoislrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                        (1|cohort)+(1|mum)+(1|dad_bio) ,
                      ziformula = ~.,
                      data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=truncated_genpois(link="log"))
tpoislrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                     (1|cohort)+(1|mum)+(1|dad_bio) ,
                   ziformula = ~.,
                   data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=truncated_poisson(link="log"))
#tweedielrsm<-glmmTMB(LRStot~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
 #                      (1|cohort)+(1|mum)+(1|dad_bio) ,
  #                   ziformula = ~.,
   #                  data=subset(offspring_lrstot,EP=="yes"&sex=="M"),family=tweedie(link="log"))
AICtab(NB2lrsm,NB1lrsm,genpoislrsm,tNB2lrsm,tNB1lrsm,tgenpoislrsm,tpoislrsm,tweedielrsm)

#best model when social father included:NB1lrsm

#when social father is excluded, genpois is best, tgenpois <1.4 difference
summary(genpoislrsm)
summary(tgenpoislrsm)

#lifespan

offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)

NB2lm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=nbinom2)
NB1lm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=nbinom1)
#compoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                  (1|cohort)+(1|mum)+(1|dad_bio) ,
#                 ziformula = ~.,
#                  data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=compois)
genpoislm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                     (1|cohort)+(1|mum)+(1|dad_bio) ,
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=genpois(link="log"))
tNB2lm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=truncated_nbinom2)
tNB1lm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=truncated_nbinom1)
#tcompoisl<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
#                    (1|cohort)+(1|mum)+(1|dad_bio) ,
#                 ziformula = ~.,
#                data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=truncated_compois)
tgenpoislm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                      (1|cohort)+(1|mum)+(1|dad_bio) ,
                    ziformula = ~.,
                    data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=truncated_genpois(link="log"))
tpoislm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=truncated_poisson(link="log"))
tweedielm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                     (1|cohort)+(1|mum)+(1|dad_bio) ,
                   ziformula = ~.,
                   data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=tweedie(link="log"))
AICtab(NB2lm,NB1lm,genpoislm,tNB2lm,tpoislm,tweedielm)
#tweedie is the only one that fits
summary(tweedielm)

#fecundity rate
offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)

table(offspring_fec$fec.rate,subset(offspring_fec$sex=="M"))

NB2m<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_fec,EP=="yes"&sex=="M"),family=nbinom2)
NB1m<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                (1|cohort)+(1|mum)+(1|dad_bio) ,
              ziformula = ~.,
              data=subset(offspring_fec,EP=="yes"&sex=="M"),family=nbinom1)
compoism<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_fec,EP=="yes"&sex=="M"),family=compois(link="log"))
genpoism<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_fec,EP=="yes"&sex=="M"),family=genpois(link="log"))
tNB2m<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_fec,EP=="yes"&sex=="M"),family=truncated_nbinom2)
tNB1m<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_fec,EP=="yes"&sex=="M"),family=truncated_nbinom1)
tcompoism<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                     (1|cohort)+(1|mum)+(1|dad_bio) ,
                   ziformula = ~.,
                   data=subset(offspring_fec,EP=="yes"&sex=="M"),family=truncated_compois)
tgenpoism<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                     (1|cohort)+(1|mum)+(1|dad_bio) ,
                   ziformula = ~.,
                   data=subset(offspring_fec,EP=="yes"&sex=="M"),family=truncated_genpois)
tpoism<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                  (1|cohort)+(1|mum)+(1|dad_bio) ,
                ziformula = ~.,
                data=subset(offspring_fec,EP=="yes"&sex=="M"),family=truncated_poisson)
tweediem<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                    (1|cohort)+(1|mum)+(1|dad_bio) ,
                  ziformula = ~.,
                  data=subset(offspring_fec,EP=="yes"&sex=="M"),family=tweedie(link="log"))
AICtab(NB2m,NB1m,compoism,genpoism,tweediem)



#no model without NAs produced (for both inclusion and exclusion of Sage)
summary(NB2m)
