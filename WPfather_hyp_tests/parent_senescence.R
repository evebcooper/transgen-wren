#hypothesis to explain why WP fathers improve with age but not EP soc fathers or EP fathers
#1. improvement is environmental, so age is associated with improved parental care/environment
#2. this environmental improvement is only associated with males who are in good condition
#3. at young ages, males are generally in good condition and being cuckolded is less reflective of condition 
#but, at old ages, survival senescence is increasing and being cuckolded is more relective of condition
#therefore, older cuckolded males are a poorer sample of the males in the population than those that sire WP

############################################
#data prep

#use offspring file
#for each offspring, calc the lifetime remaining of each parent

offspring1$Bage<-as.numeric(offspring$Bage)
offspring1$lifespanB<-as.numeric(offspring$lifespanB)

sen.par<- offspring1 %>%
  mutate(dTimeM=lifespanM-Mage) %>%
  mutate(dTimeB=lifespanB-Bage) %>%
  mutate(dTimeS=lifespanS-Sage) %>%
  filter(dTimeS>-1) %>%
  filter(dTimeB>-1) %>%
  mutate(death.yearS=ifelse(dTimeS>0,0,1))
mean(subset(sen.par$dTimeS,sen.par$EPb==0),na.rm=TRUE)
mean(subset(sen.par$dTimeS,sen.par$EPb==1),na.rm=TRUE)
#time till death is slightly lower for successful doms
#contrary to what we'd predict
mean(subset(sen.par$dTimeB,sen.par$EPb==1),na.rm=TRUE)
#time till death is the lowest for EP fathers
#NB we want to see if there's an age related effect so this is irrelevant


#prediction: offspring fitness ~ social father time until death + EP
#we expect offspring to do worse when their social dad is going to die soon

#weight
#continous time until death
weight.d<-lmer(weight~weight.age+pre1992+dTimeS+dTimeM+EPb+Jincube+(1|cohort)+(1|dad_soc)+(1|mum),data=sen.par)
summary(weight.d)
#no significant effect

#effect of death in the next year
weight.d2<-lmer(weight~weight.age+pre1992+death.yearS+EPb+Jincube+(1|cohort)+(1|dad_soc)+(1|mum),data=sen.par)
summary(weight.d2)
#still no effect

#survival
#continous time to death
surv.d<-glmer(independent~dTimeS+dTimeM+EPb+(1|cohort)+(1|dad_soc)+(1|mum),data=sen.par,family="binomial")
summary(surv.d)
#no effect of social father
#for some reason a significant positive effect of mother death which makes no sense 
#this effect is not sex specific

#continous time to death
surv.d2<-glmer(independent~death.yearS+EPb+(1|cohort)+(1|dad_soc)+(1|mum),data=sen.par,family="binomial")
summary(surv.d2)
#a negative effect on survival although p = 0.048
#this effect becomes NS when Jincube is included

#prediction 2: father's time until death ~ EPb:Sage + WPb:Sage 
#we expect successful dominants age to less strongly predict their time until death
#a more negative effect of EPb:Sage is expected
mean(sen.par$dTimeS,na.rm=TRUE)
var(sen.par$dTimeS,na.rm=TRUE)

death.time<-glmer(dTimeS~EPb:Sage+WPb:Sage+(1|cohort)+(1|dad_soc),family="poisson",data=sen.par)
summary(death.time)
#they look about the same
#if anything, there's a very slightly stronger slope for Sage:WPb, opposite to prediction