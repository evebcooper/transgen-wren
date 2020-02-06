offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
offspringCOM <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_bornCOM.csv")

offspringL<-offspring %>%
  filter(!is.na(lifespan))

hist(offspring$lifespan)
library(glmmTMB)
library(bbmle)

#graph raw effects of parental age on lifespan
#graph raw Sage data means
offspring$lifespan<-as.numeric(offspring$lifespan)
meanAge <- offspring %>%
  #filter(EP=="yes") %>%
  filter(fate=="Died")%>%
  #filter(!is.na(LRStot)) %>%
  group_by(Bage) %>%
  summarise(fledged=mean(fledged),inde=mean(independent),lifespan=mean(lifespan),LRStot=mean(LRStot))
plot(meanAge$LRStot)

####
#model selection
######
long.pois<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspringL,family=poisson)
long.nb1<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   data=offspringL,family=nbinom1)
long.nb2<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspringL,family=nbinom2)
long.poisT<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   data=offspringL,family=truncated_poisson)
long.nb1T<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspringL,family=truncated_nbinom1)
long.nb2T<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspringL,family=truncated_nbinom2)
AICtab(long.pois,long.nb1,long.nb2,long.poisT,long.nb1T,long.nb2T)

summary(long.nb2T)


#####
#WP and EP
#####
#linear
long.all<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~.,
                data=offspringL,family=truncated_nbinom2)
summary(long.all)
#NB the signs of prob model are switched (description below correct)
#significant negative interaction between Bage and EP in prob. part of model
#positive effect of Bage is lessened when offspring is EP
#no signficance in conditional part of model

#no interaction
long.alla<-glmmTMB(lifespan~Mage+Bage+EP+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~.,
                  data=offspringL,family=truncated_nbinom2)
summary(long.all)

#not controlling for EP
long.all1<-glmmTMB(lifespan~Mage+Bage+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~Mage+Bage+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                  data=offspringL,family=truncated_nbinom2)
summary(long.all1)
#no signficant effect when  you do not control for EP



#####
#lifespan - only individuals that made it to independence
#####

long.inde<-glmmTMB(lifespan~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   ziformula = ~Mage+Bage*EP+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                   data=subset(offspringL,independent==1),family=truncated_nbinom2)
summary(long.inde)
#model does not converge when maternal/paternal lifespans are included so excluded here
#no significant effects
#suggests that the significant interaction above is driven by survival between banding and independence, and not survival after


######
#WP only
########

longWP<-glmmTMB(lifespan~Mage+Bage+Jincube+lifespanB+lifespanM+(1|cohort)+(1|mum)+(1|dad_bio),
                   ziformula = ~.,
                   data=subset(offspringL,EP=="no"),family=truncated_nbinom2)
summary(longWP)
#probability:
#nothing significant
#number:
#dad's lifespan positively effects offspring lifespan (p=0.04)

#interaction between dad age and dad lifespan
longWP1<-glmmTMB(lifespan~Mage+Jincube+lifespanB*Bage+(1|cohort)+(1|mum)+(1|dad_bio),
                ziformula = ~Mage+lifespanB*Bage+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                data=subset(offspringL,EP=="no"),family=truncated_nbinom2)
summary(longWP1)
#nothing significant

######
#EP only
########

longEP<-glmmTMB(lifespan~Mage+Bage+Sage+Jincube+lifespanB+lifespanM+lifespanS+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                ziformula = ~.,
                data=subset(offspringL,EP=="yes"),family=truncated_nbinom2)
summary(longEP)
#nothing significant
#negative effect of bio dad age on probability of reaching 1 approaching significance (p=0.09)

#interaction between Bage and Mage
longEPx<-glmmTMB(lifespan~Mage*Bage+Sage+Jincube+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
                ziformula = ~Mage*Bage+Sage+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                data=subset(offspringL,EP=="yes"),family=truncated_nbinom2)
summary(longEPx)
#probability: significant negative interaction
#positive effect of Mage and Bage is lessened when both are old

#interaction between Bage offspring sex
offspringSex<-offspring %>%
  filter(sex!="U")
longEPx.s<-glmmTMB(lifespan~Mage+Bage*sex+Sage+Jincube+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~Mage+Bage*sex+Sage+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspringSex,EP=="yes"),family=truncated_nbinom2)
summary(longEPx.s)
#no interaction

#interaction between Mage and offspring sex
longEPx.s1<-glmmTMB(lifespan~Mage*sex+Bage+Sage+Jincube+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~Mage*sex+Bage+Sage+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   data=subset(offspringSex,EP=="yes"),family=truncated_nbinom2)
summary(longEPx.s1)
#model could not estimate

#interaction between Sage and Mage
longEPx2<-glmmTMB(lifespan~Mage*Sage+Bage+Jincube+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
                 ziformula = ~Mage*Sage+Bage+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
                 data=subset(offspringL,EP=="yes"),family=truncated_nbinom2)
summary(longEPx2)
#no interaction

#interaction between Sage and Bage
longEPx3<-glmmTMB(lifespan~Mage+Sage*Bage+Jincube+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  ziformula = ~Mage+Sage*Bage+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspringL,EP=="yes"),family=truncated_nbinom2)
summary(longEPx3)
#no interaction

#interaction between Bage and incubation date
longEPx4<-glmmTMB(lifespan~Mage+Sage+Jincube*Bage+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  ziformula = ~Mage+Sage+lifespanB+Jincube*Bage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspringL,EP=="yes"),family=truncated_nbinom2)
summary(longEPx4)
#no interaction


#######
#table results
########

tab_model(long.alla,long.all,longEP,longWP,transform=c(NULL),show.icc=FALSE,title = "Lifespan",
          dv.labels =c( "All offspring","All offspring - interaction", "Extra-pair offspring","Within-pair offspring"))

