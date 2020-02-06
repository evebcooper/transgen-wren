#analysis with estimated aged birds included

offspringES <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_bornES_COM.csv")

#####
#effect of bio dad age on offspring survival
#####
#linear
inde_survDades<-glmer(independent~Mage+Bage+(1|cohort)+(1|mum)+(1|dad_bio),
                    data=offspringES, family="binomial")
summary(inde_survDades)

fledge_survDades<-glmer(fledged~Mage+Bage+(1|cohort)+(1|mum)+(1|dad_bio),
                      data=offspringES, family="binomial")
summary(fledge_survDades)


fledge_survDades2<-glmer(fledged~poly(Mage,2)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_bio),
                        data=offspringES, family="binomial")
summary(fledge_survDades2)


########################

