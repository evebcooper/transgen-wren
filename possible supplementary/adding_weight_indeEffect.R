#survival to independence but including weight as a co-variate
#purpose: is the positive effect of WP father age on survival due to it's effect on weight?  
#must also control for weight age and pre1992

#base model
inde.sd<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.sd)
#weight
weight.nh<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.nh)


#including weight and other covariates
inde.w<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+weight+weight.age+pre1992+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.w)
#WP Bage is still signficant
#maternal lifespan still significant
#weight also highly significant


#including weight but not other covariates
inde.w1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                EPb+Jincube+weight+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
              data=offspring, family="binomial")
summary(inde.w1)
#again, WP Bage is still significant and so is weight

