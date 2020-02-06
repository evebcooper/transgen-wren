#average annual fecundity
#include only individuals that survived to age one (recruited)

offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)%>%
  filter(sex!="U")%>%
  filter(independent==1)

hist(offspring_fec$fec.rate)
summary(subset(offspring_fec$fec.rate>0,offspring_fec$sex=="F"))

#EP*Bage effect: males
FecxBm<-glmmTMB(fec.rate~Mage+Bage*EP+Sage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,data=subset(offspring_fec,sex=="M"),family=compois(link="log"))
summary(FecxBm)
#convergence issues, NaNs produced

#EP*Bage effect: females
FecxBf<-glmmTMB(fec.rate~Mage+Bage*EP+Sage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                ziformula = ~.,data=subset(offspring_fec,sex=="F"),family=compois(link="log"))
plot(offspring_fec$fec.rate,offspring_fec$Bage)
summary(FecxBf)
#convergence issues, AIC NA
#no significant interaction between Bage x EP
#sig positive effect of sage on both portions of model


#EP*Sage effect: males
FecxSm<-glmmTMB(fec.rate~Mage+Bage+EP*Sage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                ziformula = ~.,data=subset(offspring_fec,sex=="M"),family=compois(link="log"))
#EP*Sage effect: females
FecxSf<-glmmTMB(fec.rate~Mage+Bage+EP*Sage+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                ziformula = ~.,data=subset(offspring_fec,sex=="F"),family=compois(link="log"))
plot(offspring_fec$fec.rate,offspring_fec$Bage)

#sex specific effects
#female
fec.EPf<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="F"),family=compois(link="log"))
summary(fec.EPf)

#male
fec.EPm<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="M"),family=compois(link="log"))
summary(fec.EPm)






#DEPRECIATED
#below results based on a messed up dataset that including individuals that didn't survive to inde
##############################################
#best model determined in ZI_model_selection.R:
#compois<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
#ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=compois)

#######
#males
#######
#ZI_model_selection could not fit a model
fec.sex<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex
                 +(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=compois)


#################
#female + male together
################
#W and B indiv. effects on annual fecundity 
fec.EP<-compois
summary(fec.EP)
#positive effect of mean Bage on fecundity number (p=0.05)
#negative effect of relative Bage on fecundity number (p=0.06)
#positive effect of relative Sage on fecundity number

#sex interactions
fec.sex<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex
                 +(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
ziformula = ~.,data=subset(offspring_fec,EP=="yes"),family=compois)
summary(fec.sex) 


#sex specific effects
#female
fec.EPf<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="F"),family=compois(link="log"))
summary(fec.EPf)
#convergence problems

#female difference between vs. individual effects
#mean age effect represents the difference between between indiv. and within indiv. effects
fec.EPfD<-glmmTMB(fec.rate~Mage+Bage+Sage+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="F"),family=compois(link="log"))
summary(fec.EPfD)


#male
fec.EPm<-glmmTMB(fec.rate~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,EP=="yes"&sex=="M"),family=compois(link="log"))
summary(fec.EPm)
#convergence problems

#male difference between vs. individual effects
#mean age effect represents the difference between between indiv. and within indiv. effects
fec.EPmD<-glmmTMB(fec.rate~Mage+Bage+Sage+MageM+BageM+SageM+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  ziformula = ~.,
                  data=subset(offspring_fec,EP=="yes"&sex=="M"),family=compois(link="log"))
summary(fec.EPmD)
