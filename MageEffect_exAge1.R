#maternal age effects excluding 1 year olds


#independence - no help
inde.m1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+weight.res+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=subset(offspring,Mage>1), family="binomial")
summary(inde.m1)
#still no effect of Mage


#weight - no help
weight.m1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,Mage>1),control=lmerControl(optimizer ="bobyqa"))
summary(weight.m1)
#again, still no effect of Mage