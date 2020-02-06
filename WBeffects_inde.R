#effect of parental age on offspring survival to independence
#comparing within subject vs. between subject effects
offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")

#combine WP and EP offspring into one model
#with inclusion of julian date of birth
indeJ<-glmer(independent~MageREL+BageREL+SageREL+MageM+BageM+SageM+Jincube+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeJ,c("theta","fixef"))
indeJ<- update(indeJ,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeJ)
#positive effect of average mother's age
#there is a significant between subject effect of mother
#mothers with longer lifespans have better offspring
#however, MageRel has a negative (NS) slope, suggesting within-individual and between-individual effects are different

#combine WP and EP offspring into one model
#without inclusion of julian date of birth
inde<-glmer(independent~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(inde,c("theta","fixef"))
inde<- update(inde,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(inde)

####################
#decide whether you want to include julian birth date in the rest of the models 

#mean age effect represents the difference between between indiv. and within indiv. effects
indeBW<-glmer(independent~Mage+Bage+MageM+BageM+Jincube+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
            data=offspring, family="binomial")
ss <- getME(indeBW,c("theta","fixef"))
indeBW<- update(indeBW,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeBW)
#significantly positive mean maternal age demonstrates that between individual effects have a significantly different slope
#than within individual effects

#this suggests longer living mothers are inherently better
#mothers do not change with age
#fathers do not change with age
#longer living fathers are not better


#EP only
indeEP<-glmer(independent~MageREL+BageREL+SageREL+MageM+BageM+SageM+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
            data=subset(offspring,EP=="yes"), family="binomial")
ss <- getME(indeEP,c("theta","fixef"))
indeEP<- update(indeEP,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(indeEP)
#cannot include incubation date because model does not converge
#for some reason relative mum age is negative now...possibly something weird happening with incube. date










