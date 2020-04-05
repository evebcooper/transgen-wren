#testing the varied effects of the interactions

###################################
#independence

#base model
inde.son6<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son6)

#base model
inde.son6a<-glm(independent~Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                   EPb+Mage:EPb+lifespanM:EPb+Mage:WPb+lifespanM:WPb
                   ,
                 data=offspring, family="binomial")
summary(inde.son6a)


#excluding extra pair offspring
inde.WPa<-glm(independent~Bage*Mage+lifespanB+lifespanM+Mage,
               data=subset(offspring, EPb==1), family="binomial")
summary(inde.WPa)

#excluding extra pair offspring
inde.WP<-glmer(independent~Mage+Bage+lifespanB+lifespanM+
                   Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                   (1|mum)+(1|cohort)+(1|dad_bio),control=glmerControl(optimizer ="bobyqa"),
                 data=subset(offspring, EPb==0), family="binomial")
summary(inde.WP)
#now there is no sig effect of Bage
#is this because the other father terms are removed, or because the EPb effect is removed?

#including extra pair offspring and fixed effect of EPb
#excluding the EP father effects
inde.all<-glmer(independent~Mage+Bage*EPb+lifespanB*EPb+lifespanM+
                 Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                 (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=subset(offspring), family="binomial")
summary(inde.all)



#what happens if I change WPb from an interger to numeric
offspring$WPb<-as.numeric(offspring$WPb)
offspring$EPb<-as.numeric(offspring$EPb)

inde.all2<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.all2)
#nothing changes

#what if I change WPb/EPb to factors?
offspringF<-offspring

offspringF$WPb<-as.factor(offspringF$WPb)
offspringF$EPb<-as.factor(offspringF$EPb)

inde.all3<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspringF, family="binomial")
summary(inde.all3)
#here is drops extra coefficients so we don't have redundant coefficients (i.e. Bage:EPb0 and Sage:EPb0 are the same so the second is dropped)
#effects appear to be the same as when EPb/WPb are numeric terms 

#removing random effects
#model is base but with dad_soc RE removed
inde.RE<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                   (1|mum)+(1|cohort)+(1|dad_bio),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.RE)
#Bage WP effect is reduced from 0.10 to 0.08 and is now NS (p=0.07)


