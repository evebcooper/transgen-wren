library(tidyr)
library(dplyr)

all_birds <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/all_birds.csv")

#create new dataset with only individuals born on study site
study_born <- all_birds %>%
  filter(caught_as == "Nestling"|caught_as=="Fledgling")


#clean up and reduce columns
study_born<-study_born%>%
  dplyr::select(ID,cohort,caught,disappeared,fate,outside_area,fledged,independent,mum,Mage,
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
                         ifelse(No.of.helpers==1,"one","twoPlus")))%>%
  mutate(helpB=ifelse(No.of.helpers>0,1,0))%>%
  rename(helpNum=No.of.helpers)

#add nest ID, incube date, helper and social incest info, as well as helper names
study_born<-left_join(study_born,select(babies,c("ID","Nest.ID.mother","Jincube","help.cat","helpNum","helpB","incest","No.hatched",
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


##########################
#adding helper age information
library(readr)
#estimate helper ages
babiesES<-babies %>%
  mutate(Age.helper.1=parse_number(as.character(Age.helper.1)))%>%
  mutate(Age.helper.2=parse_number(as.character(Age.helper.2)))%>%
  mutate(Age.helper.3=parse_number(as.character(Age.helper.3)))%>%
  mutate(Age.helper.4=parse_number(as.character(Age.helper.4)))%>%
  mutate(Age.helper.5=parse_number(as.character(Age.helper.5)))

#make numeric
babiesES$Age.helper.1<-as.numeric(babiesES$Age.helper.1)
babiesES$Age.helper.2<-as.integer(babiesES$Age.helper.2)
babiesES$Age.helper.3<-as.integer(babiesES$Age.helper.3)
babiesES$Age.helper.4<-as.integer(babiesES$Age.helper.4)
babiesES$Age.helper.5<-as.integer(babiesES$Age.helper.5)

#remove crazy estimates
babiesES<-babiesES%>%
  mutate(Age.helper.1=ifelse(Age.helper.1==-7,NA,Age.helper.1))%>%
  mutate(Age.helper.1=ifelse(Age.helper.1==-8,NA,Age.helper.1))%>%
  mutate(Age.helper.1=ifelse(Age.helper.1==12,NA,Age.helper.1))%>%
  mutate(Age.helper.2=ifelse(Age.helper.2==-19,NA,Age.helper.2))%>%
  mutate(Age.helper.2=ifelse(Age.helper.2==0,NA,Age.helper.2))%>%
  mutate(Age.helper.3=ifelse(Age.helper.3==-11,NA,Age.helper.3))
  
#rename
babiesES<-babiesES %>%
  rename(help1age=Age.helper.1,help2age=Age.helper.2,help3age=Age.helper.3,help4age=Age.helper.4,help5age=Age.helper.5)

#recode helper being a son into numeric
babiesES<-babiesES %>%
  mutate(help1Mom=ifelse(Helper.1...with.mum=="Mum",1,NA))%>%
  mutate(help2Mom=ifelse(Helper.2...with.mum=="Mum",1,NA))%>%
  mutate(help3Mom=ifelse(Helper.3...with.mum=="Mum",1,NA))%>%
  mutate(help4Mom=ifelse(Helper.4...with.mum=="Mum",1,NA))%>%
  mutate(help5Mom=ifelse(Helper.5...with.mum=="Mum",1,NA))

#create a unique row for each helper and only keep helpers that are sons
helperSons <- babiesES %>%
  gather(helperKey,helpID, Helper.1:Helper.5,na.rm=TRUE) %>%
  select(ID,helperKey,helpID,help1Mom:help5Mom,help1age:help5age)%>%
  mutate(helpID=na_if(helpID,""))
helperSon1<-filter(helperSons,!is.na(help1Mom))
helperSon2<-filter(helperSons,!is.na(help2Mom))
helperSon3<-filter(helperSons,!is.na(help3Mom))
helperSon4<-filter(helperSons,!is.na(help4Mom))
helperSon5<-filter(helperSons,!is.na(help5Mom))

helperSon1<-helperSon1 %>%filter(helperKey=="Helper.1")%>%select(ID,help1age)
helperSon2<-helperSon2 %>%filter(helperKey=="Helper.2")%>%select(ID,help2age)
helperSon3<-helperSon3 %>%filter(helperKey=="Helper.3")%>%select(ID,help3age)
helperSon4<-helperSon4 %>%filter(helperKey=="Helper.4")%>%select(ID,help4age)
helperSon5<-helperSon5 %>%filter(helperKey=="Helper.5")%>%select(ID,help5age)

helperSon<-left_join(helperSon1,helperSon2)
helperSon<-left_join(helperSon,helperSon3)
helperSon<-left_join(helperSon,helperSon4)
helperSon<-left_join(helperSon,helperSon5)

#find mean age of help sons for each ID
helpSonSum<-helperSon %>%
  group_by(ID)%>%
  summarise(helpSonMean=mean(c(help1age,help2age,help3age,help4age,help5age),na.rm=TRUE),
    helpSonMin=min(c(help1age,help2age,help3age,help4age,help5age),na.rm=TRUE),
    helpSonMax=max(c(help1age,help2age,help3age,help4age,help5age),na.rm=TRUE))

#left join study_born to mean son helper age by ID
study_born<-left_join(study_born,helpSonSum)  


#summarize by baby ID and find mean max and sum ages of helpers
helpAgeSum<-babiesES %>%
  group_by(ID) %>%
  summarise(helpMeanAge=mean(c(help1age,help2age,help3age,help4age,help5age),na.rm=TRUE),
        helpMaxAge=max(c(help1age,help2age,help3age,help4age,help5age),na.rm=TRUE),
        helpMinAge=min(c(help1age,help2age,help3age,help4age,help5age),na.rm=TRUE),
        helpSonNum=sum(c(help1Mom,help2Mom,help3Mom,help4Mom,help5Mom),na.rm=TRUE))
       

#leftjoin helper age info to study_born
study_born<-left_join(study_born,helpAgeSum,by=c("ID"="ID"))

table(study_born$helpMinAge)

#fix error where individuals with no helpers are falsely coded as having 3 1 year old helpers
study_born <-study_born %>%
  mutate(helpMeanAge=ifelse(helpB==1,helpMeanAge,0))%>%
  mutate(helpMaxAge=ifelse(helpB==1,helpMaxAge,0))%>%
  mutate(helpMinAge=ifelse(helpB==1,helpMinAge,0))

#if helpB = 1, replace helpMaxAge=-inf with NA and helpMinAge =inf with NA
study_born<-study_born %>%
  mutate(helpMaxAge=ifelse(helpB==1 & helpMaxAge==-Inf,NA,helpMaxAge)) %>%
  mutate(helpMinAge=ifelse(helpB==1 & helpMinAge==Inf,NA,helpMinAge))
  
  
####################
#helper lifespans
#everyone's lifespan is in parent_long


#need to create a unique row for each helper
helpers <- babiesES %>%
  gather(helperKey,helpID, Helper.1:Helper.5,na.rm=TRUE) %>%
  select(ID,helperKey,helpID)%>%
  mutate(helpID=na_if(helpID,""))
helpers<-na.omit(helpers)

#leftjoin lifespan to helpers by helpID = ID  to the helpers file
helpers<-left_join(helpers,select(parent_long,ID,lifespan),by=c("helpID" ="ID"))

#for each baby (ID) in helpers, calculate the mean, max and sum lifespans
helpers_longSum <- helpers %>%
  group_by(ID)%>%
  summarise(longMean=mean(lifespan,na.rm=TRUE),
      longMax=max(lifespan,na.rm=TRUE))


#change NaNs/-inf/0 to 0
helpers_longSum<- helpers_longSum %>%
  mutate(longMean=recode(longMean,"NaN"=0))%>%
  mutate(longMax=recode(longMax,"-Inf"=0))



#left_join helpers_longsum to studyborn
study_born<-left_join(study_born,helpers_longSum,by=c("ID"="ID"))


#if helpB = 0, make all helper stats =0 (so these individuals aren't removed from analysis)
study_born <-study_born %>%
  mutate(longMean=ifelse(helpB==1,longMean,0))%>%
  mutate(longMax=ifelse(helpB==1,longMax,0))

#if helpB = 1, replace longMax=0 with NA
study_born<-study_born %>%
  mutate(longMax=ifelse(helpB==1 & longMax==0,NA,longMax))


######################
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


#categorical helpers relate and unrelate
study_born<-study_born %>%
  mutate(helpSonCat=ifelse(helpSonNum==0,"none",
                         ifelse(helpSonNum==1,"one","twoPlus")))%>%
  mutate(helpUnrelateCat=ifelse(helpUnrelateNum==0,"none",
                           ifelse(helpUnrelateNum==1,"one","twoPlus")))

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
