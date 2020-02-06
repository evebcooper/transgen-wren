#male survival from banding to recruitment
#includes only males because female fate during this time is uncertain
#lifespan >0 indicates they lived until their first breeding season


male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")


table(male.recruit$recruit)
#approx 25% recruited (834)

#measuring recruitment from independence
#rather than banding
male.recruitInde<- offspring %>%
  filter(sex=="M") %>%
  filter(independent==1) %>%
  #filter(fate!="Dispersedoutside")%>%
  #filter(fate!="Unknown") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))

#males recruiting from independent
table(male.recruitInde$recruit)
#834 (60%) recruited, leaving 546 to die between independence and recruitment



#including all males banded
#no help
recruit<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+
                 (1|cohort)+(1|mum/dad_soc)+(1|dad_bio),
               data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruit)
#maternal lifespan positively effects recruitment probability (p<0.01)
#Mage negatively effects recruitment probability (p=0.04)


#adding help
recruitH<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+help.cat+
                 (1|cohort)+(1|mum/dad_soc)+(1|dad_bio),
               data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
ss <- getME(recruitH,c("theta","fixef"))
recruitH<- update(recruitH,start=ss,control=glmerControl(optCtrl=list(maxfun=10e6)))
summary(recruitH)
#model doesnt converge but maternal lifespan  and Mage still significant still significant
#helpers are not important





#including only males that reached independence
#no help
recruit.inde<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               data=male.recruitInde, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruit.inde)
#singular fit
#Mage and maternal lifespan NS which tells us it's driven primarily by the effects on survival during fledging  



#remove some variables to improve fit
recruit.inde1<-glmer(recruit~Mage+lifespanM+
                      EPb+Jincube+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                      (1|cohort)+(1|mum),
                    data=male.recruitInde, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruit.inde1)
#no more singular fit but mom terms are still non-significant