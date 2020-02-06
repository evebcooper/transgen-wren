library(cAIC4)
fledgeEP2<-glmer(fledged~poly(Mage,2)+poly(Sage,2)+poly(Bage,2)+(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                 data=subset(offspring,EP=="yes"), family="binomial")

stepcAIC(fledgeEP2,trace = TRUE, direction = "backward", data = offspring)


offspringEP<-offspring %>%
  filter(EP=="yes")

fledge.tot<-glmer(fledged~poly(Mage,2)*sex+poly(Sage,2)*sex+poly(Bage,2)*sex
                  +(1|cohort)+(1|mum)+(1|dad_soc)+(1|dad_bio),
                 data=offspringEP, family="binomial")
summary(fledge.tot)

stepcAIC(fledge.tot,numberOfSavedModels=5, trace = TRUE, direction = "backward", data = offspring)

fledge.test<-glmer(fledged~poly(Mage,1)+poly(Bage,1)
                  +(1|cohort),
                  data=offspringEP, family="binomial")
summary(fledge.test)
stepcAIC(fledge.test,numberOfSavedModels=5, trace = TRUE, direction = "backward", data = offspring)
