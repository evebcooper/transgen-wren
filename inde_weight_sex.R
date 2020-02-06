############
#sex-specific survival to independence 

offspring <- offspring %>%
  filter(sex!="U") %>%
  filter(sire!="WG")

#sex interactions
indeSEX<-glmer(independent~Mage*sex+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+EPb+Jincube+help.cat+
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
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sire!="WG"),control=lmerControl(optimizer ="bobyqa"))
summary(weightSEX)
#significant interaction between sex and Mage


#male specific
weightM<-lmer(weight~Mage+EPb+Jincube+help.cat+Bage:EPb+Sage:EPb+Bage:WPb+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sex=="M"),control=lmerControl(optimizer ="bobyqa"))
summary(weightM)
#no paternal age effects (so EP Bage effect is lost)
#NS positive effect of Mage

#male - REL and M
weight.RELm<-lmer(weight~MageREL+MageM+EPb+Jincube+help.cat+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sex=="M"),control=lmerControl(optimizer ="bobyqa"))
summary(weight.RELm)
#positive effect of relative WP Bage
#boderline positive effect of mean Mage

#female specific
weightF<-lmer(weight~Mage+EPb+Jincube+help.cat+Bage:EPb+Sage:EPb+Bage:WPb+weight.age+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=subset(offspring,sex=="F"),control=lmerControl(optimizer ="bobyqa"))
summary(weightF)
#boderline (p=0.10) negative effect of EP Bage
#NS negative effect of Mage

#female - REL and M
weight.RELf<-lmer(weight~MageREL+MageM+EPb+Jincube+help.cat+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+weight.age+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspring,sex=="F"),control=lmerControl(optimizer ="bobyqa"))
summary(weight.RELf)
#nothing significant




