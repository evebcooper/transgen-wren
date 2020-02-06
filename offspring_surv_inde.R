offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
offspringCOM <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_bornCOM.csv")

#graph raw Sage data means

meanAge <- offspring %>%
  filter(EP=="yes") %>%
  group_by(Sage) %>%
  summarise(fledged=mean(fledged),inde=mean(independent))
plot(meanAge$inde)

#graph raw Bage data means

meanAge <- offspring %>%
  filter(EP=="yes") %>%
  group_by(Bage) %>%
  summarise(fledged=mean(fledged),inde=mean(independent))
plot(meanAge$inde)

########
#offspring to independence
########################

#####
#WP and EP
#####
#linear
inde.all<-glmer(independent~Mage+Bage*EP+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                    data=offspring, family="binomial")
summary(inde.all)
#significant interaction between dad age and EP
#in EP offspring  there is a negative effect of dad age, but in WP offspring there is a positive effect




#quadratric
inde2<-glmer(independent~poly(Mage,2)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                     data=offspringCOM, family="binomial")
summary(inde2)
#no quad


#interact with sex
inde.sex<-glmer(independent~Bage*sex+(1|cohort)+(1|mum)+(1|dad_bio),
                     data=subset(offspring,sex!="U"), family="binomial")
summary(inde.sex)
#no effect

#####
#EP
######

indeEP<-glmer(independent~Mage+Bage+Sage+Jincube+lifespanM+lifespanB+lifespanS+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,EP=="yes"), family="binomial")
summary(indeEP)
#NS negative effect of bio dad age
#NS negative effect of social dad age
#model failed to converge so try something simpler

#simplified by removing social father
#model converges
indeEPs<-glmer(independent~Mage+Bage+lifespanM+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
              data=subset(offspring,EP=="yes"), family="binomial")
summary(indeEPs)
#negative effects of maternal age and bio dad age approach significance

#simplified by removing bio father
#model converges
indeEPs1<-glmer(independent~Mage+Sage+lifespanM+lifespanS+Jincube+(1|cohort)+(1|mum)+(1|dad_soc),
               data=subset(offspring,EP=="yes"), family="binomial")
summary(indeEPs1)
#no effect of social father age (p=0.99)

#poly Sage
indeEP2<-glmer(independent~Mage+Bage+poly(Sage,2)+Jincube+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=subset(offspringCOM,EP=="yes"), family="binomial")
summary(indeEP2)
#significant quad effect of Sage



#####
#WP
######

indeWP<-glmer(independent~Mage+Bage+lifespanM+lifespanB+Jincube+(1|cohort)+(1|mum)+(1|dad_bio),
              data=subset(offspring,EP=="no"), family="binomial")
summary(indeWP)
#positive effect of dad age approaches significance  (model failed to converge)

#simplify model by removing bday
indeWPs<-glmer(independent~Mage+Bage+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
              data=subset(offspring,EP=="no"), family="binomial")
summary(indeWPs)
#model converges
#positive effect of Bage NS (p=0.38)

#####
#each term quadratic
#####

summary(inde_survm2<-glmer(independent~poly(Mage,2)+poly(Sage,1)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                           data=subset(offspring,EP=="yes"), family="binomial"))
summary(indesurvm2)

summary(inde_survs2<-glmer(independent~poly(Mage,1)+poly(Sage,2)+poly(Bage,1)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                           data=subset(offspring,EP=="yes"), family="binomial"))
summary(indesurvs2)
#############
#in EP matings, young and old social fathers improve offspring survival to independence

summary(inde_survb2<-glmer(independent~poly(Mage,1)+poly(Bage,2)+poly(Sage,1)+(1|cohort)+(1|mum)+(1|dad_bio),
                           data=subset(offspring,EP=="yes"), family="binomial"))
summary(inde_survb2)
