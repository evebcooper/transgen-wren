library(sjPlot)
offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")
male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")


#################################################
#weight
summary(weight.ha<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                          lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+helpNum+helpMeanAge:helpB+
                          (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                        data=offspring,control=lmerControl(optimizer ="bobyqa")))
summary(weight.ha)


tab_model(weight.ha,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Pair [yes]","Incubation Date",'Age at Weighing',
                        'Pre-1992',"Clutch Size","Number of Helpers","Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan","Mean Helper Age"
          ),order.terms = c(1,2,3,10,13,11,14,12,15,16,9,4,5,6,7,8))

#############################################################
#survival
inde.ha<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+helpMeanAge:helpB+helpNum+
                 (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(inde.ha) 

tab_model(inde.ha,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Pair [yes]","Incubation Date","Number of Helpers",
                        "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan","Mean Helper Age"
          ),order.terms = c(1,2,3,7,10,8,11,9,12,13,6,4,5))

##########################################################################
#recruitment
recruitHAa<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpNum+helpMeanAge:helpB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHAa)

tab_model(recruitHAa,transform =NULL,show.icc=FALSE,digits=3,
          pred.labels=c("Intercept","Mother Age",'Mother Lifespan',"Extra-Pair [yes]","Incubation Date","Number of Helpers",
                        "Extra-pair Genetic Father Age", "Cuckolded Social Father Age", "Within-pair Father Age",
                        "Extra-pair Genetic Father Lifespan", "Cuckolded Social Father Lifespan", "Within-pair Father Lifespan","Mean Helper Age"
          ),order.terms = c(1,2,3,7,10,8,11,9,12,13,6,4,5))




