weight.quad<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+I(Bage^2):WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.quad)
#no quadractic WP father age effect



inde.quad<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+I(Bage^2):WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+weight.res+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.quad)
#no quadractic WP father age effect