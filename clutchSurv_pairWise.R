


EP<- offspring %>%
  filter(EP=="yes") %>%
  filter(sire!="WG")
WP<- offspring %>%
  filter(EP=="no")
  


EP<- EP %>% 
  group_by(Nest.ID.mother)%>%
  sample_n(1)
WP<- WP %>% 
  group_by(Nest.ID.mother)%>%
  sample_n(1)
EP<-semi_join(EP,WP,by="Nest.ID.mother")
WP<-semi_join(WP,EP,by="Nest.ID.mother")
chickPairs<-inner_join(EP,WP,by="Nest.ID.mother")

chickPairs<-chickPairs %>%
  select(Nest.ID.mother,IDep=ID.x,IDwp=ID.y,cohort=cohort.x,indeEP=independent.x,mum=mum.x,Mage=Mage.x,
         dad_EP=dad_bio.x,EPage=Bage.x,sexEP=sex.x,MageM=MageM.x,MageREL=MageREL.x,EPageM=BageM.x,
         EPageREL=BageREL.x,Jincube=Jincube.x,help.cat=help.cat.x,EPweight=weight.x, weight.age=weight.age.x,
         indeWP=independent.y,indeWP=independent.y,
         dad_WP=dad_bio.y,WPage=Bage.y,sexWP=sex.y,WPageM=BageM.y,
         WPageREL=BageREL.y,WPweight=weight.y)
chickPairs<-chickPairs%>%
  mutate(sexdif=ifelse(sexEP==sexWP,"no","yes"))%>%
  mutate(weightdif=WPweight-EPweight) %>%
  mutate(survdif=indeWP-indeEP)%>%
  mutate(survdif1=survdif+1)

hist(chickPairs$weightdif)
table(chickPairs$survdif1)
mean(chickPairs$survdif1)
var(chickPairs$survdif1)

#change in weight
chickW<-lmer(weightdif~WPage+EPage+Mage+help.cat+Jincube+(1|cohort)+(1|mum)+(1|dad_WP)+(1|dad_EP),data=chickPairs)
summary(chickW)
#singular fit


#change in weight
summary(chickW<-lm(weightdif~WPage+EPage+Mage+EPageM+WPageM,data=chickPairs))



#change in weight
summary(chickW<-lm(weightdif~EPage+WPage+Mage+help.cat,data=subset(chickPairs,sexdif=="no")))


summary(chickS<-lm(survdif~EPage+WPage+Mage,data=chickPairs))

#binary change in survival
chickPairs<-chickPairs %>%
  mutate(indeB=ifelse(survdif==1,1,ifelse(survdif==-1,0,NA)))

summary(chickS<-glm(indeB~EPage+WPage+Mage+sexWP+sexEP,data=chickPairs))

#survival - compois

comChicks<-glmmTMB(survdif1~EPage+WPage+Mage+(1|mum),data=chickPairs,family=compois())
summary(comChicks)

poisChicks<-glmmTMB(survdif1~EPage+WPage+Mage+(1|mum),data=chickPairs,family=poisson())
summary(poisChicks)


