#lifespan amongst those that survive to independence

offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)

#best model according to ZI_model_selection.R
tNB2l<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom2)

#overall W and B effects
lifespan.EP<-tNB2l
summary(lifespan.EP)
#results are weird
#positive effect of relative Bage on probability of any lrs
#borderline signficant negative effect of mean Bage on any LRS

#sex interactions
lifespan.sex<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,
               data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom2)
summary(lifespan.sex)

#only Bage no Sage
bage.lrs<-glmmTMB(lifespan~MageREL+BageREL+MageM+BageM+(1|cohort)+(1|mum)+(1|dad_bio),
               ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom2)
summary(bage.lrs)
#effects are NS when you don't include Sage

#difference W and B indiv. effects
#mean age effect represents the difference between between indiv. and within indiv. effects
life.EPd<-glmmTMB(lifespan~Mage+Bage+Sage+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"),family=truncated_nbinom2)
summary(life.EPd)



#effects in females
life.EPf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_nbinom2)
summary(life.EPf)

#female difference W and B indiv. effects
life.EPfD<-glmmTMB(lifespan~Mage+Bage+Sage+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_nbinom2)
summary(life.EPfD)

#effects in males
life.EPm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"&sex=="m"),family=truncated_nbinom2)
summary(life.EPm)

#male difference W and B indiv. effects
life.EPmD<-glmmTMB(lifespan~Mage+Bage+Sage+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=truncated_nbinom2)
summary(life.EPmD)





