offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")


#########
#effect of age on offspring weight
##########
offspring<-offspring%>%
  filter(sex!="U")

hist(offspring$weight)
mean(offspring$weight,na.rm=TRUE)
sd(offspring$weight,na.rm=TRUE)


#all offspring
weight.all<-lmer(weight~Mage+Bage*EP+Jincube+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring)
ss <- getME(weight.all,c("theta","fixef"))
weight.all<- update(weight.all,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
summary(weight.all)
#highly significant interaction

#all offspring sage xEP
weight.allb<-lmer(weight~Mage+Bage+Sage*EP+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring)
summary(weight.allb)
#highly significant interaction

weight.alla<-lmer(weight~Mage+Bage+EP+Jincube+lifespanM+lifespanB+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring)
summary(weight.alla)

#EP only
weight.EP<-lmer(weight~Mage+Bage+weight.age+Sage+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,EP=="yes"))
summary(weight.EP)
#NS neg effect of Bage (p=0.09)
#weirdly Sage doesn't have any effect in EP mating (p=0.97)
#n= 2968

#EP only - REL vs. mean
weight.EPa<-lmer(weight~MageREL+BageREL+SageREL+weight.age+MageM+BageM+SageM+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,EP=="yes"))
summary(weight.EPa)
#NS neg effects of both BageREL and BageM

#WP only
weight.WP<-lmer(weight~Mage+Bage+Jincube+weight.age+
                  (1|cohort)+(1|mum)+(1|dad_bio),
                data=subset(offspring,EP=="no"))
summary(weight.WP)
#Bage is positive here (p=0.05) 
#n=2107 (effect: 0.022)

#WP only REL and mean
weight.WPa<-lmer(weight~Mage+BageREL+BageM+Jincube+weight.age+
                  (1|cohort)+(1|mum)+(1|dad_bio),
                data=subset(offspring,EP=="no"))
summary(weight.WPa)
#both bage rel and mean are NS pos



#graph
tab_model(weight.alla,weight.all,weight.EP,weight.WP,show.icc=FALSE,title = "Weight at Banding",
          dv.labels =c( "All offspring","All offspring - interaction","Extra-pair offspring","Within-pair offspring"))

