library(sjPlot)
offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")


#################################################
#weight

summary(weight.ha1<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                           lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+
                           helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                           (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                         data=offspring,control=lmerControl(optimizer ="bobyqa")))
summary(weight.ha1)

tab_model(weight.ha1,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Pair Dummy [yes]","Incubation Date",'Age at Weighing',
                'Pre-1992',"Clutch Size","Related Helper Presence [1]","Related Helper Presence [2+]","Unrelated Helper Presence [1]","Unrelated Helper Presence [2+]",
                  "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan","Mean Helper Age"
          ),order.terms = c(1,6,7,8,5,4,2,3,15,18,14,18,13,17,9,10,11,12,19))

#############################################################
#survival
inde.son6<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son6)

tab_model(inde.son6,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Pair Dummy [yes]","Incubation Date",
                        "Related Helper Presence [1]","Related Helper Presence [2+]","Unrelated Helper Presence [1]","Unrelated Helper Presence [2+]",
                        "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan","Mean Helper Age"
          ),order.terms = c(1,5,4,2,3,12,15,11,14,10,13,6,7,8,9,16))

##########################################################################
#recruitment
recruitHA3<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonCat+helpUnrelateCat+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA3)

tab_model(recruitHA3,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Pair Dummy [yes]","Incubation Date",
                        "Related Helper Presence [1]","Related Helper Presence [2+]","Unrelated Helper Presence [1]","Unrelated Helper Presence [2+]",
                        "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan","Mean Helper Age"
          ),order.terms = c(1,5,4,2,3,12,15,11,14,10,13,6,7,8,9,16))





table(offspring$helpMeanAge,offspring$helpSonB)
table(offspring$helpUnrelateB,offspring$helpSonB,offspring$helpMeanAge)
mean(subset(offspring$helpMeanAge,offspring$helpUnrelateB==1,offspring$helpMeanAge),na.rm=TRUE)

helpstats<-offspring %>%
  group_by(helpMeanAge) %>%
  summarise(chicks=n(),sons=sum(helpSonNum),unrelated=sum(helpUnrelateNum),survival=mean(independent))
helpstats<-mutate(helpstats,prop.son=sons/(sons+unrelated))
helpstats<-filter(helpstats,chicks>20)





