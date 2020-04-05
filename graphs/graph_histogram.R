#create histogram of number of each at each age:
#mothers, each father type, mean helper age

library(ggplot2)
library(dplyr)

offspringCOM<-offspring %>%
  filter(!is.na(Mage))%>%
  filter(!is.na(Bage))%>%
  filter(!is.na(Sage)) %>%
  filter(!is.na(helpMeanAge))%>%
  filter(!is.na(lifespanM))%>%
  filter(!is.na(lifespanB))%>%
  filter(!is.na(lifespanS)) %>%
  filter(!is.na(Jincube))
  
BageWP <-offspringCOM %>%
  filter(EPb==0)%>%
  select(ID,age=Bage)%>%
  mutate(type="aBageWP",typeB="dad")
BageEP <-offspringCOM %>%
  filter(EPb==1)%>%
  select(ID,age=Bage)%>%
  mutate(type="bBageEP",typeB="dad")
SageEP <-offspringCOM %>%
  filter(EPb==1)%>%
  select(ID,age=Sage)%>%
  mutate(type="cSageEP",typeB="dad")
mom <-offspringCOM %>%
  select(ID,age=Mage)%>%
  mutate(type="Mage",typeB="Mage")
help <-offspringCOM %>%
  select(ID,age=helpMeanAge)%>%
  mutate(type="help",typeB="help")

ageHist<-rbind(mom,help,BageWP,BageEP,SageEP)
ageHist$type<-as.factor(ageHist$type)
ageHist$typeB<-as.factor(ageHist$typeB)

ageHist_fathers<-ageHist %>%
  group_by(type,age)%>%
  filter(typeB=="dad")%>%
  summarise(total=n()) 
  
#histogram of ages by each type of father
ggplot(ageHist_fathers, aes(x=age,y=total,fill=type))+
  geom_bar(position="dodge", stat="identity") +
  scale_x_continuous(breaks=c(1:12))+
  coord_cartesian(xlim=c(1:10),ylim=c(0:500))+
  xlab("Age (years)")+
  ylab("Count")+
  scale_fill_discrete(name="Father Type",labels=c("Within-pair","Extra-pair Sire","Extra-pair Cuckolded"))+
  theme_minimal()


####################
#histogram of ages for mothers, each of the three fathers, and helpers all together

BageWP <-offspringCOM %>%
  filter(EPb==0)%>%
  select(ID,age=Bage)%>%
  mutate(type="BBageWP",typeB="dad")
BageEP <-offspringCOM %>%
  filter(EPb==1)%>%
  select(ID,age=Bage)%>%
  mutate(type="CBageEP",typeB="dad")
SageEP <-offspringCOM %>%
  filter(EPb==1)%>%
  select(ID,age=Sage)%>%
  mutate(type="DSageEP",typeB="dad")
mom <-offspringCOM %>%
  select(ID,age=Mage)%>%
  mutate(type="AMage",typeB="Mage")
help <-offspringCOM %>%
  select(ID,age=helpMeanAge)%>%
  mutate(age=round(age)) %>%
  mutate(type="Ehelp",typeB="help")

ageHist<-rbind(mom,help,BageWP,BageEP,SageEP)
ageHist$type<-as.factor(ageHist$type)

ageHist_all<-ageHist %>%
  group_by(type,age)%>%
  summarise(total=n()) %>%
  filter(age!=0)


ggplot(ageHist_all, aes(x=age,y=total,fill=type))+
  geom_bar(position="dodge", stat="identity") +
  #geom_density(aes(x=age,y=total,colour=type),stat="identity",alpha=0) +
  scale_x_continuous(breaks=c(1:12))+
  coord_cartesian(xlim=c(1,10),ylim=c(0,1300))+
  xlab("Age (years)")+
  ylab("Count")+
  scale_fill_discrete(name="",labels=c("Mother","Father: Within-pair","Father: Extra-pair Sire","Father: Extra-pair Cuckolded","Helpers"))+
  theme_minimal()




