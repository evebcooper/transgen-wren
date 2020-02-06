library(tidyr)


all_birds <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/all_birds.csv")

#select important columns
LRSdat<-all_birds%>%
  select(ID,cohort,caught,disappeared,fate,outside_area,fledged,independent,mum,Mage,
         dad_soc,Sage,dad_bio,Bage,sex,LRStot,LRSfledge,LRSinde)

####add in lifespans of offspring
#seperate the last two digits of the 'disappeared' column

LRSdat<-separate(LRSdat,disappeared,sep="-",into=c("dis.day","dis.month","dis.year"))

LRSdat$dis.year<-as.numeric(LRSdat$dis.year)
LRSdat$cohort<-as.character(LRSdat$cohort)
LRSdat$cohort<-as.numeric(LRSdat$cohort)

LRSdat <-LRSdat %>%
  mutate(dis.year=ifelse(dis.year>84,dis.year+1900,dis.year+2000))%>%
  mutate(lifespan=ifelse(fate=="Died",dis.year-cohort,NA))


#####add in lifespans of all the parents
parent_long<-separate(all_birds,disappeared,sep="-",into=c("dis.day","dis.month","dis.year"))

parent_long$dis.year<-as.numeric(parent_long$dis.year)
parent_long$cohort<-as.character(parent_long$cohort)
parent_long$cohort<-as.numeric(parent_long$cohort)

parent_long<-parent_long %>%
  mutate(dis.year=ifelse(dis.year>84,dis.year+1900,dis.year+2000))%>%
  mutate(lifespan=ifelse(fate=="Died",dis.year-cohort,NA))

parent_longM <-parent_long %>%
  select(mum=ID,lifespanM=lifespan) 
parent_longB <-parent_long %>%
  select(dad_bio=ID,lifespanB=lifespan) 
parent_longS <-parent_long %>%
  select(dad_soc=ID,lifespanS=lifespan) 


LRSdat<-left_join(LRSdat,parent_longM,by="mum")

LRSdat<-left_join(LRSdat,parent_longB,by="dad_bio")
LRSdat<-left_join(LRSdat,parent_longS,by="dad_soc")


#calculate mean age for each parent
mean_mum<-LRSdat %>%
  group_by(mum)%>%
  summarise(MageM=(mean(Mage,na.rm=TRUE)))

mean_dadB<-LRSdat %>%
  group_by(dad_bio)%>%
  summarise(BageM=mean(Bage,na.rm=TRUE))
mean_dadS<-LRSdat %>%
  group_by(dad_soc)%>%
  summarise(SageM=mean(Sage,na.rm=TRUE))
LRSdat<-left_join(LRSdat,mean_mum,by="mum")
LRSdat<-left_join(LRSdat,mean_dadB,by="dad_bio")
LRSdat<-left_join(LRSdat,mean_dadS,by="dad_soc")

#calculate relative age of each parent
#this is the within-subject effect
LRSdat<-LRSdat%>%
  mutate(MageREL=Mage-MageM)%>%
  mutate(BageREL=Bage-BageM)%>%
  mutate(SageREL=Sage-SageM)

#add qualifier if dad_soc != dad_bio
LRSdat<-LRSdat %>%
  mutate(EP=ifelse(dad_soc==dad_bio,"no","yes"))  


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

#make a dataframe with a row for each baby from raw nests
#first make a seperate data frame for each baby
nest1<- nestRAW %>%
  select(-Baby.2,-Baby.3,-Baby.4,-Baby.5)%>%
  rename(ID=Baby.1)
nest2<- nestRAW %>%
  select(-Baby.1,-Baby.3,-Baby.4,-Baby.5)%>%
  rename(ID=Baby.2)
nest3<- nestRAW %>%
  select(-Baby.2,-Baby.1,-Baby.4,-Baby.5)%>%
  rename(ID=Baby.3)
nest4<- nestRAW %>%
  select(-Baby.2,-Baby.3,-Baby.1,-Baby.5)%>%
  rename(ID=Baby.4)
nest5<- nestRAW %>%
  select(-Baby.2,-Baby.3,-Baby.4,-Baby.1)%>%
  rename(ID=Baby.5)
#now merge all 5 data tables and delete empty ID rows
babies<-rbind(nest1,nest2,nest3,nest4,nest5) %>%
  rowwise() %>%
  filter(ID!="",ID!="*")
babies<-babies%>%
  rename(incest = Social.incest..pair.is.mother.son.)
babies<-babies%>%
  mutate(help.cat=ifelse(No.of.helpers==0,"none",
                         ifelse(No.of.helpers==1,"one","twoPlus")))

#add nest ID, incube date, helper and social incest info
LRSdat<-left_join(LRSdat,select(babies,c("ID","Nest.ID.mother","Jincube","help.cat","incest")))


#add individual chick weights
Pulli_weights <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/Pulli_weights.txt", header=FALSE)
Pulli_weights<-rename(Pulli_weights,ID=V1,weight=V20,weight.age=V10)
LRSdat<-left_join(LRSdat,select(Pulli_weights,c("ID","weight","weight.age")))
LRSdat<-LRSdat%>%
  mutate(weight=ifelse(weight<1,NA,weight)) %>%
  mutate(weight.age=ifelse(weight.age<1,NA,weight.age))

#create a binary variable for WP and EP
LRSdat<-LRSdat %>%
  mutate(EPb=ifelse(EP=="yes",1,0))%>%
  mutate(WPb=ifelse(EP=="no",1,0))

#excluding mother-son pairs from analysis 
#because there is correlation between Sage and incest, and sons may act differently from unrelated Sage
LRSdat<-LRSdat %>%
  filter(incest=="No")

#clean up the file
LRSdat<-LRSdat%>%
  select(-caught,-dis.day,-dis.month,-dis.year)



#add indication if sire is a helper
LRSdat<-mutate(LRSdat, NestID=Nest.ID.mother)
nestRAW<-mutate(nestRAW, NestID=Nest.ID.mother)
nest.help<-nestRAW%>%
  select(Helper.1,Helper.2,Helper.3,Helper.4,Helper.5,NestID)

LRSdat<-LRSdat %>%
  left_join(nest.help, by=c("NestID"))

LRSdat <- LRSdat %>%
  mutate(help.ep=case_when(dad_bio == Helper.1 ~ "yes",
                          dad_bio == Helper.2 ~ "yes",
                          dad_bio == Helper.3 ~ "yes",
                          dad_bio == Helper.4 ~ "yes",
                          dad_bio == Helper.5 ~ "yes"))
LRSdat<-LRSdat%>%
  mutate(sire.help=ifelse(is.na(son.ep),"no","yes"))

LRSdat<-LRSdat %>% select(-Helper.1,-Helper.2,-Helper.3,-Helper.4,-Helper.5)

#remove individuals that are not known dead
LRSdat<-LRSdat%>%
  filter(fate=="Died") 



write.csv(LRSdat,file="LRSdat.csv")


