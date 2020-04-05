#models with helperage effects

#checking for sex-specific interactions

#weight

summary(weightSEX<-lmer(weight~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+lifespanB:EPb*sex+lifespanS:EPb*sex+lifespanB:WPb*sex+
                          lifespanM*sex+EPb+Jincube+weight.age+pre1992+No.hatched+helpSonB+helpUnrelateB+helpMeanAge:helpB*sex+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=subset(offspring,sex!="U"),control=lmerControl(optimizer ="bobyqa")))
summary(weightSEX)

#positive interaction between sex and maternal lifespan

#sex specific models
#males
summary(weightM<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                        lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMeanAge:helpB+
                        (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=subset(offspring,sex=="M"),control=lmerControl(optimizer ="bobyqa")))
summary(weightM)
#no effect of maternal lifespan on male weight (positive NS)

#females
summary(weightF<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                        lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMeanAge:helpB+
                        (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                      data=subset(offspring,sex=="F"),control=lmerControl(optimizer ="bobyqa")))
summary(weightF)
#no effect of maternal lifespan on female weight  (negative NS)


####################
#survival

indeSEX<-glmer(independent~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+lifespanB:EPb*sex+lifespanS:EPb*sex+lifespanB:WPb*sex+lifespanM*sex+
                 EPb+Jincube+helpMeanAge:helpB*sex+helpNum+
                 (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=subset(offspring,sex!="U"), family="binomial")
summary(indeSEX) 
#no sig interactions


#males
indeM<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+helpMeanAge:helpB+helpNum+
                 (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=subset(offspring,sex=="M"), family="binomial")
summary(indeM) 


#females
indeF<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
               EPb+Jincube+helpMeanAge:helpB+helpNum+
               (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
             data=subset(offspring,sex=="F"), family="binomial")
summary(indeF) 








