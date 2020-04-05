#graphs show mothers with lifespan of 1 have very low offspring survival and recruitment
#rerun surv and recruit models excluding mothers with a lifespan of 1 
#if lifespanM is still significant, the mothers with lifespan 1 are not driving this effect
#models show that mothers that only live one year are not driving the effect!


inde.son5<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=subset(offspring,lifespanM>1), family="binomial")
summary(inde.son5)
 
#there is still a positive  effect of lifespanM and a negative effect of Mage
#lifespan effect drops from 0.10 to 0.08

#recruitment
male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")

recruitHA2<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=subset(male.recruit,lifespanM>1), family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA2)

#lifespanM is still highly significant, Mage still significant
#coefficient barely drops (0.104 to 0.096)