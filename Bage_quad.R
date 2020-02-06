#try quadratic effect of bio dad's age


#banding weight
offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
#all offspring
weight.all<-lmer(weight~Mage+poly(Bage,2)+Jincube+lifespanM+lifespanB+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,!is.na(Bage)))
summary(weight.all)
#NS

#EP only
weight.EP<-lmer(weight~Mage+poly(Bage,2)+Sage+Jincube+lifespanM+lifespanB+lifespanS+weight.age+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,!is.na(Bage)& EP=="yes"))
summary(weight.EP)
#NS

#EP only
weight.WP<-lmer(weight~Mage+poly(Bage,2)+Sage+Jincube+lifespanM+lifespanB+weight.age+
                  (1|cohort)+(1|mum)+(1|dad_bio),
                data=subset(offspring,!is.na(Bage)& EP=="no"))
summary(weight.WP)
#NS


#survival to independence 
#quadratric
inde2<-glmer(independent~poly(Mage,1)+poly(Bage,2)+lifespanB+lifespanM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspringCOM, family="binomial")
summary(inde2)
#NS


#LRS
LRS2<-glmmTMB(LRStot~Mage+poly(Bage,2)+Jincube+lifespanM+lifespanB+lifespanS+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             ziformula = ~.,
             data=subset(offspring_lrs1,!is.na(Bage)),family=truncated_nbinom1)
summary(LRS2)
#no quad effect

#lifespan
long2<-glmmTMB(lifespan~Mage+poly(Bage,2)+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
              ziformula = ~.,
              data=subset(offspring_lrs1,!is.na(Bage)),family=truncated_nbinom2)
summary(long2)
#NS


