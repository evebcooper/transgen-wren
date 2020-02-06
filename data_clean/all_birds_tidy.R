library(dplyr)


#import all birds
#Andrew gave me updated file 24/09/2019
all_birdsRAW <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/data/all_birdsRAW.csv")


#change annoying values to NA
all_birdsED <- all_birdsRAW %>%
  mutate(dad_bio=na_if(dad_bio,"*")) %>%
  mutate(dad_bio=na_if(dad_bio,"")) %>%
  mutate(dad_bio=na_if(dad_bio,"epc")) %>%
  mutate(dad_bio=na_if(dad_bio,"epc-NS")) %>%
  mutate(dad_soc=na_if(dad_soc,"*")) %>%
  mutate(dad_soc=na_if(dad_soc,"")) %>%
  mutate(mum=na_if(mum,"*")) %>%
  mutate(mum=na_if(mum,"")) %>%
  mutate(Bage=na_if(Bage,"*")) %>%
  mutate(Bage=na_if(Bage,"")) %>%
  mutate(Bage=na_if(Bage,"epc")) %>%
  mutate(Bage=na_if(Bage,"epc-NS")) %>%
  mutate(Bage=na_if(Bage,"#N/")) %>%
  mutate(Bage=na_if(Bage,"0+")) %>%
  mutate(Bage=na_if(Bage,"99")) %>%
  mutate(Sage=na_if(Sage,"*")) %>%
  mutate(Sage=na_if(Sage,"")) %>%
  mutate(Sage=na_if(Sage,"#N/")) %>%
  mutate(Mage=na_if(Mage,"*")) %>%
  mutate(Mage=na_if(Mage,"#N/")) %>%
  mutate(Mage=na_if(Mage,"0")) %>%
  mutate(Mage=na_if(Mage,"0+")) %>%
  mutate(Mage=na_if(Mage,""))
  
#remove "-" from ages with this typo
all_birdsED$Mage<-gsub(pattern="-", replacement="",all_birdsED$Mage)
all_birdsED$Sage<-gsub(pattern="-", replacement="",all_birdsED$Sage)
all_birdsED$Bage<-gsub(pattern="-", replacement="",all_birdsED$Bage) 


#change estimated ages to certain ages and save as own datafile
library(readr)
all_birdsES<-mutate(all_birdsED, Mage=parse_number(Mage))
all_birdsES<-mutate(all_birdsES, Sage=parse_number(Sage))
all_birdsES<-mutate(all_birdsES, Bage=parse_number(Bage))

write.csv(all_birdsES,file="all_birdsES.csv")  

#Make ages NA if there are not known with certainty
all_birdsED$Mage<-as.numeric(all_birdsED$Mage)
all_birdsED$Sage<-as.numeric(all_birdsED$Sage)
all_birdsED$Bage<-as.numeric(all_birdsED$Bage)

all_birds<-all_birdsED
write.csv(all_birds,file="all_birds.csv")  


 

write.csv(study_bornCOM,file="study_bornCOM.csv")  



