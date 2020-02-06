
offspring_long<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex!="U")%>%
  filter(lifespan>0)%>%
  filter(sire!="WG")


#best model according to offspring_lifespanAdults
Lnb2<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=subset(offspring.long,EP=="yes"))
summary(Lnb2)
#no significant interactions with sex


#base model
life<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life)
#no effects

#check adding help.cat, Jincube
life2<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+Jincube+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life2)
#Jincube is significant but doesn't change anything in the model

#check sex interaction in base model
glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))
life.sex<-glmmTMB(lifespan~Mage*sex+EPb+Bage:EPb*sex+Sage:EPb*sex+Bage:WPb*sex+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life.sex)
#no sex interaction 
###################################################################################################
#males
life.m<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.m)
#no sig ffects

#females
life.f<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=subset(offspring_long,sex=="F"))
summary(life.f)
#boderline negative (p=0.08) effect of WP Bage


#REL vs. mean - males
life.relM<-glmmTMB(lifespan~MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.relM)
#no sig effects

#REL vs. mean - females
life.relF<-glmmTMB(lifespan~MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+
                    +(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   family=nbinom2(),data=subset(offspring_long,sex=="F"))
summary(life.relF)
#a negative effect of WP mean Bage 










