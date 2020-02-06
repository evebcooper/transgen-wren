#########
#bring in nest info
#######
nestRAW <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/nestRAW.csv")
427-244
#183
mean(nestRAW$Julian.incubation)
#252

#clean and z-transform julian incube
nestRAW <- nestRAW %>%
  rename(Jincube=Julian.incubation) %>%
  mutate(Jincube=ifelse(Jincube>243,Jincube,NA)) %>%
  mutate(Jincube= (Jincube-252)/183)


nests<- nestRAW %>%
  select("Nest.ID.mother","Season.of.nest","Mother.ID","Age.of.mother","Dominant.ID", "Dominant.Age","Jincube","No.of.eggs",
         "No.hatched", "No.fledged","Fledged.Yes.or.No","No.of.young.independent..4.weeks.","No.legitimate","No.extra.pair",
         "Social.incest..pair.is.mother.son.","No.of.helpers","Territory.ID")%>%
  filter(Social.incest..pair.is.mother.son.=="No")%>%
  rename(nestID=Nest.ID.mother,cohort=Season.of.nest,mumID=Mother.ID,Mage=Age.of.mother, domID=Dominant.ID,
         Dage=Dominant.Age,eggs=No.of.eggs,hatched=No.hatched,fledge=No.fledged,fledgeB=Fledged.Yes.or.No,
         inde=No.of.young.independent..4.weeks.,legit=No.legitimate,EP=No.extra.pair,helpers=No.of.helpers,territory=Territory.ID)%>%
  mutate(prop.legit=legit/hatched)%>% #this is a conservative estimate since not all that hatch neccessarily fledge
  mutate(EPprob=ifelse(EP>0,1,0))%>%
  mutate(WPprob=ifelse(legit>0,1,0))%>%
  mutate(hatch.inde=inde/hatched)%>% #this is the prop. of the clutch reaching inde
  filter(hatch.inde<1.1)%>% #some numbers are clearly impossible so remove
  mutate(hatch.indeB=ifelse(hatch.inde>0,1,0))%>%
  #(no. legitimate - no. EP)
  mutate(WP_EP = legit-EP)
  

#remove "-" from ages with this typo
nests$Mage<-gsub(pattern="-", replacement="",nests$Mage)
nests$Dage<-gsub(pattern="-", replacement="",nests$Dage)

nests$Dage<-as.numeric(nests$Dage)
nests$Mage<-as.numeric(nests$Mage)


#calculate relative WP and EP offspring weights
offspring<-offspring %>%
  rename(nestID = Nest.ID.mother)

offspring.W<-offspring %>%
  filter(!is.na(nestID))%>%
  mutate(pre1992B=ifelse(pre1992=="yes",1,0))%>%
  group_by(nestID)%>%
  summarise(chicks=n(),EP=sum(EP=="yes"),WP=sum(EP=="no"),pre1992=mean(pre1992B),help.cat=mean(help.catN),
            weight.age=mean(weight.age)) 

offspring.weightEP<-offspring %>%
  filter(!is.na(nestID))%>%
  filter(EP=="yes")%>%
  group_by(nestID)%>%
  summarise(EPchicks=n(),EPweightSUM=sum(weight))%>%
  mutate(EPweightM=EPweightSUM/EPchicks)

offspring.weightWP<-offspring %>%
  filter(!is.na(nestID))%>%
  filter(EP=="no")%>%
  group_by(nestID)%>%
  summarise(WPchicks=n(),WPweightSUM=sum(weight))%>%
  mutate(WPweightM=WPweightSUM/WPchicks)


offspring.W<-left_join(offspring.W,offspring.weightEP)
offspring.W<-left_join(offspring.W,offspring.weightWP)

offspring.W<-offspring.W %>%
  select(-WPchicks,-EPchicks)%>%
  mutate(WP_EPweight=WPweightM-EPweightM)

#connect WP_EP weight difference to each nest

nests<-nests%>%
  left_join(offspring.W,by="nestID")
#something weird is happening where the offspring.W file has over 1000 missing rows
#its because the offspring file doesn't contain these nestIDs
#I dont know why but if additional sample size is needed should try to figure this one out
  
nests<-nests %>%
  filter(Dage<13) %>%
  filter(Mage<11) %>%
  filter(Mage>0)


table(nests$Mage)
table(nests$Dage)
table(nests$hatch.inde)
hist(nests$hatch.indeB)

table(nests$prop.legit)
table(nests$legit)
table(nests$EP)
sum(nests$hatched)
sum(nests$fledge)
table(nests$EPprob)
#5750 fledge, 10659 hatch, but only 6973 assigned parentage (since this occurs between hatch and fledge)
#option 1: reduce sample to only fledged indiv., so we can be confident of EP
#option 2: keep everyone and do binary prob. of any EP young

#only fledged nests included
#the effect of dom age (in EP and WP chicks) on the probability of reaching independence
nest.glm<-glmer(hatch.indeB~Dage*EPprob+Mage+(1|mumID)+(1|domID)+(1|cohort),family="binomial",
                control=glmerControl(optimizer ="bobyqa"),data=nests)
summary(nest.glm)
#the effect of Dage does not change dependent on whether it's an EP or a WP

#ratio of EP to WP offspring in response to Dage
hist(nests$WP_EP.inde,breaks=100)
shapiro.test(nests$WP_EP.inde)
nest1.glm<-lmer(WP_EP~Dage+Mage+(1|mumID)+(1|domID)+(1|cohort),data=nests)
summary(nest1.glm)

#difference between WP and EP weight as a function of Dage
#prediction: WP_EP should increase with increasing Dage
summary(weightC)
#no significant effect

#Dage on overall WP weight
weightC.wp<-lmer(WPweightR~Dage*Mage+(1|mumID)+(1|domID)+(1|cohort),data=nests)
summary(weightC.wp)
#NS

#Dage on overall EP weight
weightC.ep<-lmer(EPweightR~Dage*Mage+(1|mumID)+(1|domID)+(1|cohort),data=nests)
summary(weightC.ep)
#NS
#significant Dage*Mage interaction
#Positive effect of Mage on EP weight is lessened by increasing Dage
#if either Mage or Dage are old, that's good for EP weight, but if both are old it's bad
#why only for EP and not WP?
#for EP, Dage will reduce offspring fitness with increasing age
#apparently this depends on Mage though...


############################################
#clutch-level analysis of independence and weight

#data sorting
offspring$independent<-as.numeric(offspring$independent)

clutch <-offspring %>%
  filter(!is.na(EP))%>%
  group_by(nestID,mum,dad_soc) %>%
  summarise(size =n(),ind= sum(independent>0),ep.ind=sum(independent>0 & EP=="yes"),
            wp.ind=sum(independent>0 & EP=="no"),wp.tot=sum(EP=="no"),ep.tot=sum(EP=="yes"),Mage=max(Mage),Sage=max(Sage),
            Bage=mean(subset(Bage,EPb==1)),cohort=mean(cohort))%>%
  mutate(dif.ind=wp.ind-ep.ind)%>%
  mutate(any.ind=ifelse(ind>0,"yes","no"))
#add binary category, if difference is greater or less than 0
clutch<-mutate(clutch, difB=ifelse(dif.ind>0,1,0))
#add ratio of number WP/number EP
clutch<-mutate(clutch, WPprop=(wp.ind/ind))
#remove rows where mother ID is na
clutch<-filter(clutch,!is.na(mum))
#proportion of WP banded that survived to independence  
clutch<-clutch%>%
  mutate(any.wp=ifelse(wp.tot>0,'yes','no'))%>%
  mutate(wp.tot.prop=wp.ind/wp.tot)

clutch.beta<-clutch %>%
  mutate(WPprop=ifelse(WPprop==0,0.0000001,ifelse(
    WPprop==1,0.9999999,WPprop))) %>%
  mutate(wp.tot.prop=ifelse(wp.tot.prop==0,0.0000001,ifelse(
    wp.tot.prop==1,0.9999999,wp.tot.prop)))
  
#####################
#data checking
hist(subset(clutch$dif.ind,clutch$any.ind=="yes"))  
mean(subset(clutch$dif.ind,clutch$any.ind=="yes"),na.rm=TRUE) 
var(subset(clutch$dif.ind,clutch$any.ind=="yes"),na.rm=TRUE) 
shapiro.test(subset(clutch$dif.ind,clutch$any.ind=="yes")) 
#not normally distributed, not poisson

###############################
#Analysis


#probability that more WP will survive
clutch.glm<-glmer(difB~Sage+Mage+(1|mum)+(1|dad_soc)+(1|cohort),
                  family="binomial",data=subset(clutch,any.ind=="yes"))
summary(clutch.glm)
#negative effect of Sage when Bage excluded
#no effect of Sage when Bage included

#number of WP - EP
clutch.lm<-lmer(dif.ind~Sage+Mage+(1|mum)+(1|dad_soc)+(1|cohort),
                  data=subset(clutch,any.ind=="yes"))
summary(clutch.lm)
#model did not converge but Sage effect is NS negative

#proportion of surviving that are WP
hist(clutch$WPprop)
mean(clutch$WPprop,na.rm=TRUE)
var(clutch$WPprop,na.rm=TRUE)
 library(glmmTMB)

##########################################################################################################
#social = WP father 
WPprop.betaS<-glmmTMB(WPprop~Sage+Mage+wp.tot+ep.tot+(1|mum)+(1|dad_soc)+(1|cohort),
                     family="beta_family",data=subset(clutch.beta,wp.tot>0 & ep.tot>0))
summary(WPprop.betaS)
#there's no effect of social father's age on the proportion WP surviving, when controlling for the clutch size and intial ratio

#########################################################################################################
#social - squared 
WPprop.betaS2<-glmmTMB(WPprop~Sage+I(Sage^2)+Mage+(1|mum)+(1|dad_soc)+(1|cohort),
                      family="beta_family",data=subset(clutch.beta,any.ind=="yes"))
summary(WPprop.betaS2)
#no quadratic effect

#effect of Sage (WP father age) on total wp offspring that survive
WPtot<-glmmTMB(wp.ind~Sage+Mage+(1|mum)+(1|dad_soc)+(1|cohort),
                      family="poisson",data=clutch.beta)
summary(WPtot)
#social father age has a negative effect on total WP survived (NS)
#NB could be because they have fewer offspring sired 

#social - effect on WP that make it to inde from the entire clutch born
WP.clutch<-glmmTMB(wp.ind/size~Sage+Mage+(1|mum)+(1|dad_soc)+(1|cohort),
               family="poisson",data=clutch.beta)
summary(WPtot)
#NS negative effect

#social - effect just on EP
EPtot<-glmmTMB(ep.ind~Sage+Mage+Bage+(1|mum)+(1|dad_soc)+(1|cohort),
               family="poisson",data=clutch.beta)
summary(EPtot)
#social father age has no effect on ep offspring survived
#EP father (bage) also has no effect on the survival of his offspring

#effect on proportion of inde offspring that are WP 
WPprop.betaS2<-glmmTMB(WPprop~Sage+Mage+(1|mum)+(1|dad_soc)+(1|cohort),
                      family="beta_family",data=subset(clutch.beta,any.ind=="yes"))
summary(WPprop.betaS2)
#no effects of social father age in model including Bage (clutches that include at least 1 EP)
#negative effects when Bage excluded

#so far you've looked at proportion of WP surviving from all surviving offspring in a clutch
#look instead at proportion of WP surviving from all WP born
#there might be a negative correlation with WP mating success and father age, so a smaller proportion of offspring will be WP,
#so less WP offspring != lower WP offspring survival

#proportion of WP banded that survive to independence

mean(subset(clutch.beta$wp.tot.prop,clutch.beta$any.wp=='yes'),na.rm=TRUE)
var(clutch.beta$wp.tot.prop,na.rm=TRUE)

WPrel<-glmmTMB(wp.tot.prop~Sage+Mage+Bage+(1|mum)+(1|dad_soc)+(1|cohort),
                       family="beta_family",data=subset(clutch.beta,any.wp=="yes"))
summary(WPrel)
#no effect of Sage

#not controlling for Bage
WPrel1<-glmmTMB(wp.tot.prop~Sage+mage+(1|mum)+(1|dad_soc)+(1|cohort),
               family="beta_family",data=subset(clutch.beta,any.wp=="yes"))
summary(WPrel1)
#still no effect of Sage

####################################################################################
#effect of Sage on weight

#use nests file from sireSuccess_EPfitness

#difference in weight between EP and WP chicks
 weight.c<-lmer(WP_EPweight~Dage+Mage+(1|mumID)+(1|cohort),data=nests)
summary(weight.c)
#no effect of dom age on weight difference between chicks

#difference in weight between EP and WP chicks
#controlling for number of WP chicks
weight.c1<-lmer(WP_EPweight~Dage+Mage+legit+(1|mumID)+(1|cohort),data=nests)
summary(weight.c1)
#still no effect


#interaction between Dage and number of legitimate chicks
#interaction would indicate the negative effect of having legitimate chicks on their relative weight lessens with Dage
weight.c2<-lmer(WP_EPweight~Mage+Dage*legit+(1|mumID)+(1|cohort),data=nests)
summary(weight.c2)
#no effect








