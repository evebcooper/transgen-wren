library(tidyr)


all_birds <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/all_birds.csv")

#create new dataset with only individuals born on study site
study_born <- all_birds %>%
  filter(caught_as == "Nestling"|caught_as=="Fledgling")


#clean up and reduce columns
study_born<-study_born%>%
  select(ID,cohort,caught,disappeared,fate,outside_area,fledged,independent,mum,Mage,
         dad_soc,Sage,dad_bio,Bage,sex,LRStot,LRSfledge,LRSinde)

####add in lifespans of offspring

#seperate the last two digits of the 'disappeared' column

study_born<-separate(study_born,disappeared,sep="-",into=c("dis.day","dis.month","dis.year"))

study_born$dis.year<-as.numeric(study_born$dis.year)
study_born$cohort<-as.character(study_born$cohort)
study_born$cohort<-as.numeric(study_born$cohort)

study_born <-study_born %>%
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


study_born<-left_join(study_born,parent_longM,by="mum")

study_born<-left_join(study_born,parent_longB,by="dad_bio")
study_born<-left_join(study_born,parent_longS,by="dad_soc")

#calculate mean age for each parent
mean_mum<-study_born %>%
  group_by(mum)%>%
  summarise(MageM=(mean(Mage,na.rm=TRUE)))

mean_dadB<-study_born %>%
  group_by(dad_bio)%>%
  summarise(BageM=mean(Bage,na.rm=TRUE))
mean_dadS<-study_born %>%
  group_by(dad_soc)%>%
  summarise(SageM=mean(Sage,na.rm=TRUE))
study_born<-left_join(study_born,mean_mum,by="mum")
study_born<-left_join(study_born,mean_dadB,by="dad_bio")
study_born<-left_join(study_born,mean_dadS,by="dad_soc")

#calculate relative age of each parent
#this is the within-subject effect
study_born<-study_born%>%
  mutate(MageREL=Mage-MageM)%>%
  mutate(BageREL=Bage-BageM)%>%
  mutate(SageREL=Sage-SageM)
  
#add qualifier if dad_soc != dad_bio
study_born<-study_born %>%
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
  mutate(JincubeRaw=Jincube)%>%
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

#add nest ID, incube date, helper and social incest info, as well as helper names
study_born<-left_join(study_born,select(babies,c("ID","Nest.ID.mother","Jincube","help.cat","incest","No.hatched",
                                                 "Helper.1","Helper.2","Helper.3","Helper.4","Helper.5")))
#indicate if Bage is a helper and offspring is EP (thus EG not EP)
study_born<-study_born %>%
  mutate(sire=ifelse(EP=="no","WP",ifelse(
    dad_bio==Helper.1,"WG",ifelse(
      dad_bio==Helper.2,"WG",ifelse(  
        dad_bio==Helper.3,"WG",ifelse(
          dad_bio==Helper.4,"WG",ifelse(
            dad_bio==Helper.5,"WG","EP"
        )))))))

#remove helper columns
study_born <- study_born %>%
  select(-Helper.1,-Helper.2,-Helper.3,-Helper.4,-Helper.5)  

#add individual chick weights
Pulli_weights <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/Pulli_weights.txt", header=FALSE)
Pulli_weights<-rename(Pulli_weights,ID=V1,weight=V20,weight.age=V10)
study_born<-left_join(study_born,select(Pulli_weights,c("ID","weight","weight.age")))
study_born<-study_born%>%
  mutate(weight=ifelse(weight<1,NA,weight)) %>%
  mutate(weight.age=ifelse(weight.age<1,NA,weight.age))

#create a binary variable for WP and EP
study_born<-study_born %>%
  mutate(EPb=ifelse(EP=="yes",1,0))%>%
  mutate(WPb=ifelse(EP=="no",1,0))



#add pre 1992 indicator
study_born<-study_born %>%
  mutate(pre1992=ifelse(cohort<1992,"yes","no"))

#excluding mother-son pairs from analysis 
#because there is correlation between Sage and incest, and sons may act differently from unrelated Sage
study_born<-study_born %>%
  filter(incest=="No")

#excluding chicks who are sired from helpers on their own territory
study_born<-study_born %>%
  filter(sire!="WG")
  
#adding a corrected value of lifespan which is accurate to sept 1 rather than just the cohort
study_born <- study_born %>%
  mutate(deathPastSep=ifelse(dis.month=="Sep",0,
                             ifelse(dis.month=="Oct",0,
                                    ifelse(dis.month=="Nov",0,
                                    ifelse(dis.month=="Sep",0,
                                           ifelse(dis.month=="Dec",0,-1))))))
study_born<-study_born %>%
  mutate(lifespanSep=lifespan+deathPastSep)



#clean up the file
study_born<-study_born%>%
  select(-caught,-dis.day,-dis.month,-dis.year,-LRSfledge,-LRSinde)

#weight residuals
#model the effects of Bage  etc. on weight to get residuals
weight.res<-lm(weight~weight.age+pre1992,
               data=study_born)
residW<-resid(weight.res)

study_bornFULL<-study_born %>%
  filter(!is.na(weight))%>%
  filter(!is.na(weight.age))

study_bornFULL<-mutate(study_bornFULL, weight.res=residW)

study_born<-left_join(study_born,select(study_bornFULL,ID,weight.res),by="ID")

#remove duplicate chicks
study_born<-study_born %>%
  distinct(ID,.keep_all=TRUE)

write.csv(study_born,file="study_born.csv")




##########################################################################################################
#create a dataset with only complete records of all parental ages known
study_bornCOM<-study_born %>%
  filter(!is.na(Mage))%>%
  filter(!is.na(Bage))%>%
  filter(!is.na(Sage)) %>%
  filter(!is.na(dad_bio))
write.csv(study_bornCOM,file="study_bornCOM.csv")







##########
#estimated ages
############

all_birdsES <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/all_birdsES.csv")

#create new dataset with only individuals born on study site
study_bornES <- all_birdsES %>%
  filter(caught_as == "Nestling"|caught_as=="Fledgling")

#remove row names
study_bornES<-study_bornES%>%
  select(ID,cohort,caught,disappeared,fate,outside_area,fledged,independent,mum,Mage,
         dad_soc,Sage,dad_bio,Bage,sex,LRStot,LRSfledge,LRSinde)

#add qualifier if dad_soc != dad_bio
study_bornES$dad_bio<-as.character(study_bornES$dad_bio)
study_bornES$dad_soc<-as.character(study_bornES$dad_soc)
study_bornES<-study_bornES %>%
  mutate(EP=ifelse(dad_soc==dad_bio,"no","yes"))


write.csv(study_bornES,file="study_bornES.csv")

#create a dataset with only complete records of all parental ages known
study_bornES_COM<-study_bornES %>%
  filter(!is.na(Mage))%>%
  filter(!is.na(Bage))%>%
  filter(!is.na(Sage)) %>%
  filter(!is.na(dad_bio))
write.csv(study_bornES_COM,file="study_bornES_COM.csv")
