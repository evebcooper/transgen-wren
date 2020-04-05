#exploring influence of maternal lifespan on survival
#how does removing maternal lifespan influence maternal age effect?


#in full model, maternal lifespan is significantly positive and maternal age NS negative
# (these numbers hold true if you add helpers or don't)


###############################
#weight
summary(weight.ha1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          EPb+Jincube+weight.age+pre1992+No.hatched+
                           helpMeanAge:helpB+helpSonB+helpUnrelateB+
                           (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                         data=offspring,control=lmerControl(optimizer ="bobyqa")))
#Mage has no effect (nothing has changed)




###################################
#survival
inde.mat<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                   EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.mat)
#OG model: mother age sig negative (p=0.03)
#mother age not significant



#####################################
#recruitment

recruit.mat<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                    EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruit.mat)
#mother age not signficant




#base model
inde.sd<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.sd)


#removal of maternal lifespan
inde.ml<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                 EPb+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.ml)
# negative effect of Mage is lessened but it still remains negative NS



#removal of maternal lifespan while including helpers
inde.ml2<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                 EPb+Jincube+help.cat+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.ml2)
#nothing interesting