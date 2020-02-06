offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
library(glmmTMB)
library(bbmle)
library(dplyr)

#######################################
#overall LRS
offspring_lrstot<- offspring %>%
  filter(fate=="Died") %>%
  filter(sex!="U")

NB2lrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                 +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=nbinom2)
NB1lrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=nbinom1)
genpoislrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                    +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=genpois(link="log"))
tNB2lrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                  +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=truncated_nbinom2)
tNB1lrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                  +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=truncated_nbinom1)
tgenpoislrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                      +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=truncated_genpois(link="log"))
tpoislrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                   +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=truncated_poisson(link="log"))
tweedielrsx<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                     +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrstot,EP=="yes"),family=tweedie(link="log"))
AICtab(NB2lrsx,NB1lrsx,genpoislrsx,tNB2lrsx,tNB1lrsx,tgenpoislrsx,tpoislrsx,tweedielrsx)

#with sex interactions
summary(NB1lrsx)
#sig interaction in prob for BageM and MageREL

####without sex interactions
#best is NB1LRS
#tgonpoislrs, tNB1lrs, NB2lrs, and tNB2lrs are all <3 dAIC from best

#######################################################
#lifespan
offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex!="U")

NB2lx<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
              +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=nbinom2)
NB1lx<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
              +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=nbinom1)
genpoislx<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                  +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=genpois(link="log"))
tNB2lx<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
               +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom2)
tNB1lx<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom1)
tgenpoislx<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                   +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=truncated_genpois(link="log"))
tpoislx<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=truncated_poisson(link="log"))
tweedielx<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                  +(1|dad_soc),ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=tweedie(link="log"))
AICtab(NB2lx,NB1lx,genpoislx,tNB2lx,tNB1lx,tgenpoislx,tpoislx,tweedielx)

#with sex interactions
#tgenpoislx is the 'best' model but does not give results,neither do other models with AIC calculated
#only model with stats calculated is tNB2lx (best model w/out sex int.), but it has no AIC
summary(tgenpoislx)
summary(tNB2lx)
#no signficant interactions (but no AIC so model is not converging)

#without sex interactions
#tNB2l is best fit
summary(tNB2l)

##################################
##########################################################################################################################
#fecundity rate
offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan) %>%
  filter(sex!="U")

NB2x<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
             +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=nbinom2)
NB1x<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
              +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=nbinom1)
compoisx<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                 +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=compois(link="log"))
genpoisx<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                  +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=genpois(link="log"))
tNB2x<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
               +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_nbinom2)
tNB1x<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
               +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_nbinom1)
tcompoisx<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                  +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_compois)
tgenpoisx<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                   +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_genpois)
tpoisx<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
               +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=truncated_poisson)
tweediex<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                 +(1|dad_soc),ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=tweedie(link="log"))
#AICtab(NB2x,NB1x,genpoisx,tNB2x,tNB1x,tgenpoisx,tpoisx,tweediex,compoisx,tcompoisx)

AICtab(NB2x,NB1x,genpoisx,tweediex,compoisx)
#with sex interactions:compoisx
summary(compoisx)
#no sex interactions

#without sex interactions
#compois is the only model where AIC can be calculated
#conditional
#borderline sig (p=0.05) negative effect of relative Bage
#boderline sig (p=0.05) positive effect of mean Bage
#positive effect of relative Sage (having an older dominant male suggests you may take over soon)
#binary
#nothing



#fecundity rate
#offset used for lifespan
#fecundity rate
offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)

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
AICtab(NB2o,NB1o,genpoiso,compoiso,tNB2o,tNB1o,tcompoiso,tgenpoiso,tpoiso,tweedieo)