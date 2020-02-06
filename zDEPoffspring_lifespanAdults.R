#lifespan models but only including individuals that survived to independence

offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
library(glmmTMB)
library(bbmle)
library(dplyr)

offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)

#all
long<-glmmTMB(lifespan~Mage+Bage+Jincube+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
                  ziformula = ~.,
                  data=offspring_lrs1,family=truncated_nbinom2)
summary(long)
#boderline positive effect (p=0.06) of Bage on survival to one year
#could just be due to having older Sage for WP improves early life survival

#EP
longEP<-glmmTMB(lifespan~Mage+Bage+Sage+Jincube+lifespanM+lifespanB+lifespanS+(1|cohort)+(1|mum)+(1|dad_bio),
              ziformula = ~.,
              data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom2)
summary(longEP)
#significantly positive effect of Bage on survival from independence -> age 1
#NB this becomes NS when controlling for Sage

#WP
longWP<-glmmTMB(lifespan~Mage+Bage+lifespanM+lifespanB+(1|cohort)+(1|mum)+(1|dad_bio),
                ziformula = ~.,
                data=subset(offspring_lrs1,EP=="no"),family=truncated_nbinom2)
summary(longWP)
#no effect of WP dad's age
