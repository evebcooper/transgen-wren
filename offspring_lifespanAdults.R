#investigating the differences in sex-specific and sex unified lifespan models
options(na.action=na.omit)


#lifespan of those that make it to maturity
offspring.long<- offspring %>%
  filter(independent==1) %>%
  filter(fate=="Died")%>%
  filter(sex!="U")%>%
  filter(lifespan>0)

table(offspring.long$lifespan)

#check for overdispersion
dispersionstats <- offspring.long %>%
  summarise(
  means = mean(lifespan),
  variances = var(lifespan),
  ratio = variances/means)
#data is pretty overdispersed

#if the data is skewed, try NB1, NB2 and do an AIC
Lnb2<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               family=nbinom2(),data=subset(offspring.long,EP=="yes"))
Lnb1<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom1(),data=subset(offspring.long,EP=="yes"))
Lgenpois<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=genpois(link="log"),data=subset(offspring.long,EP=="yes"))
Lcompois<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  family=compois(link="log"),data=subset(offspring.long,EP=="yes"))

AICtab(Lnb2,Lnb1,Lgenpois)
#best model Lnb2

summary(Lnb2)
#no signficiant interactions
#run compois, beta_family,betabinomial and tweedie overnight to check


#run base model
LS<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+MageM+BageM+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=subset(offspring.long,EP=="yes"))
summary(LS)
#no significant effects
  
  
#interaction with EP
LSxEP<-glmmTMB(lifespan~Mage+Bage*EP+
              (1|cohort)+(1|mum)+(1|dad_bio),
            family=nbinom2(),data=offspring.long)
summary(LSxEP)
#no significant effects


#all offspring REL
life.int<-glmmTMB(lifespan~MageREL+BageREL*EP+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  family=nbinom2(),data=offspring.long)
summary(life.int)
#no interaction


#all offspring - relative vs. mean
life.int1<-glmmTMB(lifespan~MageREL+BageREL*EP+BageM*EP+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   family=nbinom2(),data=offspring.long)
summary(life.int1)
#no interactions
  

#interaction with EP and sex
LSxEPxS<-glmmTMB(lifespan~Mage+Bage*EP*sex+
                 (1|cohort)+(1|mum)+(1|dad_bio),
               family=nbinom2(),data=offspring.long)
summary(LSxEPxS)
#no significant effects