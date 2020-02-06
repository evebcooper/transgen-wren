############
#sex-specific survival to independence 

offspring <- offspring %>%
  filter(sex!="U")

#sex interactions
indeSEX<-glmer(independent~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+
                 lifespanB:EPb*sex+lifespanS:EPb*sex+lifespanB:WPb*sex+lifespanM*sex+
                 EPb+Jincube+help.cat+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspring,sire!="WG"), family="binomial",
               control=glmerControl(optimizer ="bobyqa"))
summary(indeSEX)
#no significant interactions

#males
indeM<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+EPb+Jincube+help.cat+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=subset(offspring,sex=="M"), family="binomial",
             control=glmerControl(optimizer ="bobyqa"))
summary(indeM)
#nothing sig (n = 2629)
#everything is facing the correct direction

#females
indeF<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+EPb+Jincube+help.cat+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=subset(offspring,sex=="F"), family="binomial",
             control=glmerControl(optimizer ="bobyqa"))
summary(indeF)
#boderline negative effect of Sage (p=0.08)


###########
#sex-specific effects on weight

#sex inteactions
weightSEX<-lmer(weight~Mage*sex+EPb+Jincube+help.cat+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+weight.age+
                  pre1992+lifespanB:EPb*sex+lifespanS:EPb*sex+lifespanB:WPb*sex+lifespanM*sex+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sire!="WG"),control=lmerControl(optimizer ="bobyqa"))
summary(weightSEX)
#significant interaction between sex and lifespanM


#male specific
weightM<-lmer(weight~Mage+EPb+Jincube+help.cat+Bage:EPb+Sage:EPb+Bage:WPb+weight.age+pre1992+
                lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sex=="M"),control=lmerControl(optimizer ="bobyqa"))
summary(weightM)
#no paternal age effects 
#NS positive effect of maternal lifespan

#male - REL and M
weight.RELm<-lmer(weight~MageREL+MageM+EPb+Jincube+help.cat+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sex=="M"),control=lmerControl(optimizer ="bobyqa"))
summary(weight.RELm)
#positive effect of relative WP Bage
#boderline positive effect of mean Mage

#female specific
weightF<-lmer(weight~Mage+EPb+Jincube+help.cat+Bage:EPb+Sage:EPb+Bage:WPb+weight.age+pre1992+
                lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=subset(offspring,sex=="F"),control=lmerControl(optimizer ="bobyqa"))
summary(weightF)
#NS negative effect of maternal lifespan

#female - REL and M
weight.RELf<-lmer(weight~MageREL+MageM+EPb+Jincube+help.cat+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+weight.age+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspring,sex=="F"),control=lmerControl(optimizer ="bobyqa"))
summary(weight.RELf)
#nothing significant


###################################
#lifespan

#check sex interaction in base model
glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))
life.sex<-glmmTMB(lifespan~Mage*sex+EPb+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  family=nbinom2(),data=offspring_long)
summary(life.sex)
#no sex interaction 







