#hypothesis: males with bad sperm sire fewer chicks and also are bad at raising chicks 
#prediction:this should create a correlation between siring success and clutch-level fitness

#need:for each nest, proportion that were sired by WP males and the clutch average weight, and survival metrics
#remove WP males that are the sons of the female

#bring in nest info
nestRAW <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/nestRAW.csv")
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


#calculate offspring relative WP and EP offspring weights
offspring<-offspring %>%
  rename(nestID = Nest.ID.mother)

offspring.W<-offspring %>%
  filter(!is.na(nestID))%>%
  group_by(nestID)%>%
  summarise(chicks=n(),EP=sum(EP=="yes")) %>%
  mutate(WP=chicks-EP)

offspring$No.hatched<-as.numeric(offspring$No.hatched)

offspring1<-offspring %>%
  mutate(pre1992B=ifelse(pre1992=="yes",1,0))

offspring.weight<-offspring1 %>%
  filter(!is.na(nestID))%>%
  group_by(nestID)%>%
  summarise(weightSUM=sum(weight),No.hatched=mean(No.hatched),pre1992=mean(pre1992B),help.cat=mean(help.catN),
            weight.age=mean(weight.age))%>%
  mutate(weightM=weightSUM/No.hatched)

#connect weight stats to each nest

nests<-nests%>%
  left_join(select(offspring.weight,nestID,pre1992,help.cat,weightM,weight.age,No.hatched),by="nestID")

nests<-nests %>%
  filter(Dage<13) %>%
  filter(Mage<11) %>%
  filter(Mage>0)

nests<-nests%>%
  mutate(legitB=ifelse(legit>0,1,0))


#does a higher prop. of legit chicks help the survival of the EP chicks in the clutch?


#use the offspring dataset to summarise the survival and weight of EP and WP chicks in each nest
nestSuccess <- offspring %>%
  group_by(nestID) %>%
  summarise(EPsurvTot=sum(ifelse(EPb==1,independent,0)),WPsurvTot=sum(ifelse(EPb==0,independent,0)),
            EPweightTot=sum(ifelse(EPb==1,weight,NA)),WPweightTot=sum(ifelse(EPb==0,weight,NA))) #sum weight, we'll get mean later

#left_join nest success to nests
#nestSuccess has more nests because social incest hasn't been removed
nests<-left_join(nests,nestSuccess)
#NAs produced in cases where no chicks made it to banding age

#remove weights that are 0 (these are cases where no chicks made it to weighing)
nests<-nests %>%
  mutate(EPweightTot=ifelse(EPweightTot>0,EPweightTot,NA))%>%
  mutate(WPweightTot=ifelse(WPweightTot>0,WPweightTot,NA))%>%
#get mean weights
  mutate(EPweightM=EPweightTot/hatched)%>%
  mutate(WPweightM=WPweightTot/hatched)%>%
  #remove rows where both legit and EP are 0 (no chicks banded/faulty data)
  filter(legit>0 | EP>0)%>%
  #get binary of if any EP or WP survived
  mutate(EPsurvB=ifelse(EP==0,NA,ifelse(EPsurvTot>0,1,0)))%>%
  mutate(WPsurvB=ifelse(legit==0,NA,ifelse(WPsurvTot>0,1,0)))

######################
#analysis

#overall mean weight of the clutch  
weight<-lmer(weightM~prop.legit+hatched+help.cat+Dage+Mage+weight.age+pre1992+Jincube+
               (1|cohort)+(1|mumID)+(1|domID),data=nests)
summary(weight)
#weight of the whole clutch is improved with the more legitimacy within the clutch
#this result is robust to controlling for helpers, Mage, Dage
#weirdly, Mage has a significant effect here despite that neither Mage or lifespanM have an effect in other model

#mean weight of the WP chicks
weightWP<-lmer(WPweightM~prop.legit+hatched+weight.age+pre1992+Jincube+
                 (1|cohort)+(1|mumID)+(1|domID),data=nests)
summary(weightWP)
#significant positive effect of siring success on WP chick weight
#this stands whether you control for absolute number of legit or number hatched 

#mean weight of the EP chicks
weightEP<-lmer(EPweightM~prop.legit+hatched+weight.age+pre1992+Jincube+
                 (1|cohort)+(1|mumID)+(1|domID),data=nests)
summary(weightEP)
#no effect on EP chicks

#males with higher siring success have heavier offspring, but it does not effect weight of EP chicks
#this suggests a genetic, rather than environmental effect

#interaction with Dage?
weightWP1<-lmer(WPweightM~prop.legit*Dage+No.hatched+weight.age+pre1992+Jincube+
                 (1|cohort)+(1|mumID)+(1|domID),data=nests)
summary(weightWP1)
#the beneficial effect of siring success does not improve with age
#suggests selective appearance that males that have high success at late ages are intrinsically better

#effect in only old males
weightWP2<-lmer(WPweightM~prop.legit+No.hatched+weight.age+pre1992+Jincube+
                  (1|cohort)+(1|mumID)+(1|domID),data=subset(nests,Dage>2))
summary(weightWP2)
#effect in only young males
weightWP3<-lmer(WPweightM~prop.legit+No.hatched+weight.age+pre1992+Jincube+
                  (1|cohort)+(1|mumID)+(1|domID),data=subset(nests,Dage<2))
summary(weightWP3)
#no, it still matters in any age group


########
#survival to independence

nests$help.cat<-as.factor(nests$help.cat)

#hatch.inde is the proportion of the clutch that hatched which reached independence
#probability of any chicks surviving to inde in response to the proportion of the clutch that's legitimate
sireB<-glmer(hatch.indeB~prop.legit+hatched+Jincube+help.cat+(1|mumID)+(1|cohort)+(1|domID),family="binomial",data=nests,
             control=glmerControl(optimizer ="bobyqa"))
summary(sireB)
#a higher proportion of the clutch being legitimate increases the survival of the whole clutch (p=0.04)
#when we control for helpers this becomes more significant (p=0.02)

#exclude cases where clutch is entirely EP or entirely WP 
#this way it matches dataset used below
sireB1<-glmer(hatch.indeB~prop.legit+hatched+Jincube+help.cat+(1|mumID)+(1|cohort)+(1|domID),family="binomial",
             data=subset(nests,EP==0 |legit ==0),
             control=glmerControl(optimizer ="bobyqa"))
summary(sireB1)
#with this smaller sample size (1162 vs. 1916), prop legit is no longer significant

#WP survival
inde.sire<-glmer(WPsurvB~prop.legit+legit+Jincube+help.cat+(1|cohort)+(1|mumID)+(1|domID),
                 family="binomial",data=nests)
summary(inde.sire)
#ES = 0.47, SE = 0.44
#when controlling for the number of legitimate chicks, the proportion of the clutch they occupy is NS 
#controlling for help.cat causes convergence problems but does not change results

#EP survival
inde.sireEP<-glmer(EPsurvB~prop.legit+EP+Jincube+help.cat+(1|cohort)+(1|mumID)+(1|domID),
                 family="binomial",data=nests)
summary(inde.sireEP)
#ES = 0.06, SE = 0.26
#clearly, although both NS, prop.legit has a greater effect on WP survival vs. EP survival
#prop.legit has no effect on the EP success when you control for the number of EP
#controlling for helpers does not change results

#see if there's an interaction with Dage
inde.sireD<-glmer(WPsurvB~prop.legit*Dage+legit+Jincube+(1|cohort)+(1|mumID)+(1|domID),
                 family="binomial",data=nests)
summary(inde.sireD)
# no convergence but no interaction


#see if there's an interaction with Mage
inde.sireM<-glmer(WPsurvB~prop.legit*Mage+legit+Jincube+(1|cohort)+(1|mumID)+(1|domID),
                  family="binomial",data=nests)
summary(inde.sireM)
# no convergence but no interaction

