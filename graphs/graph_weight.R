#weight graphs
library(modelr)
library(ggplot2)
library(ggeffects)

#base model
weight<-lmer(weight~MageC+BageC:EPb+BageC:WPb+SageC:EPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
               lifespanM+EPb+Jincube+weight.age+pre1992+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspring)
summary(weight)

#put 9 and 10 year olds at 8 
offspring<-offspring %>%
  mutate(BageC=ifelse(Bage==10,9,Bage))

#dataframe
weightDat<-offspring %>%
  select(Mage,BageC,Bage,Sage,lifespanB,lifespanS,lifespanB,lifespanM,Jincube,weight.age,pre1992,weight,EPb,
         cohort, mum, dad_bio,dad_soc,WPb)
weightDat<-na.omit(weightDat)

###################
#WP father 
#re-run model without the effect of WP dad age and WP dad lifespan 
#BageC rounded
weightG<-lmer(weight~Mage+BageC:EPb+Sage:EPb+lifespanB:EPb+lifespanS:EPb+
                lifespanM+EPb+Jincube+weight.age+pre1992+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=offspring)
summary(weightG)
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(resid=resid(weightG)) %>%
  mutate(weightCor=weight-resid)
#WP father age
#bootstrap to get predictions and CI
newdat<-new_data(weight,terms=c("Bage"),condition=c(EP=="no"))
bb<-(bootMer(weight,function(x)predict(x,re.form=NA,newdata=newdat),nsim=1000,type="parametric"))
#summarise age raw data to 9 rows
weightDat1 <- weightDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),weight=mean(weightCor))
#add predictions  
weightDat1$lci <- apply(bb$t, 2, quantile, 0.025)
weightDat1$uci <- apply(bb$t, 2, quantile, 0.975)
weightDat1$weightpred <- apply(bb$t, 2, quantile, 0.5)

#WP father lifespan
#bootstrap to get predictions and CI
newdat<-new_data(weight,terms=c("lifespanB"),condition=c(EPb=0,WPb=1))
bb1<-(bootMer(weight,function(x)predict(x,re.form=NA,newdata=newdat),nsim=1000,type="parametric"))
#summarise age raw data 
weightDat.span <- weightDat%>%
  filter(EPb==0)%>%
  group_by(lifespanB)%>%
  summarise(num=n(),weight=mean(weightCor))
#add predictions  
weightDat.span$lci <- apply(bb1$t, 2, quantile, 0.025)
weightDat.span$uci <- apply(bb1$t, 2, quantile, 0.975)
weightDat.span$weightpred <- apply(bb1$t, 2, quantile, 0.5)

###################
#EP bio father 
#put 9 and 10 year olds at 8 
offspring<-offspring %>%
  mutate(BageC=ifelse(Bage==10,9,Bage))
#re-run model without the effect of WP dad age and WP dad lifespan 
weightEPB<-lmer(weight~Mage+BageC:WPb+Sage:EPb+lifespanB:WPb+lifespanS:EPb+
                lifespanM+EPb+Jincube+weight.age+pre1992+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=offspring)
summary(weightEPB)
#calc residuals and add to raw data
weightDat<-weightDat %>%
  mutate(residEPB=resid(weightEPB)) %>%
  mutate(weightCorEPB=weight-residEPB)
#EP bio father age
#bootstrap to get predictions and CI
newdat<-new_data(weight,terms=c("BageC"),condition=c(EPb=1,WPb=0))
bbEPB<-(bootMer(weight,function(x)predict(x,re.form=NA,newdata=newdat),nsim=50,type="parametric"))
#summarise age raw data to 9 rows
weightDatEPB <- weightDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(num=n(),weight=mean(weightCorEPB))
#add predictions  
weightDatEPB$lci <- apply(bbEPB$t, 2, quantile, 0.025)
weightDatEPB$uci <- apply(bbEPB$t, 2, quantile, 0.975)
weightDatEPB$weightpred <- apply(bbEPB$t, 2, quantile, 0.5)
#WP father lifespan
#bootstrap to get predictions and CI
newdat<-new_data(weight,terms=c("lifespanB"),condition=c(EPb=1,WPb=0))
bb1EPB<-(bootMer(weight,function(x)predict(x,re.form=NA,newdata=newdat),nsim=50,type="parametric"))
#summarise age raw data 
weightDat.spanEPB <- weightDat%>%
  filter(EPb==1)%>%
  group_by(lifespanB)%>%
  summarise(num=n(),weight=mean(weightCorEPB))
#add predictions  
weightDat.spanEPB$lci <- apply(bb1EPB$t, 2, quantile, 0.025)
weightDat.spanEPB$uci <- apply(bb1EPB$t, 2, quantile, 0.975)
weightDat.spanEPB$weightpred <- apply(bb1EPB$t, 2, quantile, 0.5)


ggplot(data=weightDat1,aes(x=BageC,y=pred))+
  geom_point(aes(x=BageC,y=weight,color="Age"))+
  geom_line(data=weightDat1,mapping=aes(x=BageC,y=pred,color="Age"))+
  geom_ribbon(mapping=aes(fill="Age",x=BageC,ymax=uci,ymin=lci),alpha=0.1)+
  #geom_point(data=weightDat.span,mapping= aes(x=lifespanB,y=weight,color="Lifespan"),shape=4)+
  #geom_line(data=weightDat.span,mapping=aes(x=lifespanB,y=weightpred,color="Lifespan"))+
  #geom_ribbon(data=weightDat.span,mapping=aes(x=lifespanB,ymax=uci,ymin=lci,fill="Lifespan"),alpha=0.1)+
  scale_x_continuous(limits=c(1,11))+
  #scale_y_continuous(limits=c(6.75,10))+
  scale_fill_manual(name = "", values = c("Age" = "darkred","Lifespan" = "darkgrey")) +
  scale_color_manual(name = "", values = c("Age" = "darkred","Lifespan" = "darkgrey")) +
  labs(title="Within-Pair Father Age",x="Years",y="Chick Weight")+
  #facet_grid(.~)+
  theme_classic()



ggplot(data=weightDatEPB,aes(x=BageC,y=weightpred))+
  geom_point(aes(x=BageC,y=weight,color="Age"))+
  geom_line(mapping=aes(x=BageC,y=weightpred,color="Age"))+
  geom_ribbon(mapping=aes(fill="Age",x=BageC,ymax=uci,ymin=lci),alpha=0.1)+
  #geom_point(data=weightDat.spanEPB,mapping= aes(x=lifespanB,y=weight,color="Lifespan"),shape=4)+
  geom_line(data=weightDat.spanEPB,mapping=aes(x=lifespanB,y=weightpred,color="Lifespan"))+
  geom_ribbon(data=weightDat.spanEPB,mapping=aes(x=lifespanB,ymax=uci,ymin=lci,fill="Lifespan"),alpha=0.1)+
  scale_x_continuous(limits=c(1,11))+
  #scale_y_continuous(limits=c(6.75,10))+
  scale_fill_manual(name = "", values = c("Age" = "darkred","Lifespan" = "darkgrey")) +
  scale_color_manual(name = "", values = c("Age" = "darkred","Lifespan" = "darkgrey")) +
  labs(title="Within-Pair Father Age",x="Years",y="Chick Weight")+
  theme_classic()




  
  

 