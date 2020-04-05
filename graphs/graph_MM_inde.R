#graphs for helper age models


#survival to independence
inde.son4<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son4)
survDat<-offspring %>%
  dplyr::select(independent,Mage,Bage,Sage,lifespanB,
                lifespanS,lifespanB,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio,dad_soc)
survDat<-na.omit(survDat)

##########
#graph - helper age
summary(inde.son4) 

X<-cbind(1,
         mean(survDat$Mage),
         mean(survDat$lifespanM),
         0.5484728,
         mean(survDat$Jincube),
         mean(survDat$helpSonB),
         mean(survDat$helpUnrelateB),
         mean(subset(survDat$Bage,survDat$EPb==1,Bage)),
         mean(subset(survDat$Sage,survDat$EPb==1,Sage)),
         mean(subset(survDat$Bage,survDat$EPb==0,Bage)),
         mean(subset(survDat$lifespanB,survDat$EPb==1,Bage)),
         mean(subset(survDat$lifespanS,survDat$EPb==1,Sage)),
         mean(subset(survDat$lifespanB,survDat$EPb==0,Bage)),
         c(1:6))
#extract var covar and fixed effect estimates
var<-vcov(inde.son4)
beta<-fixef(inde.son4)
#predictions (logistic scale)
pred<-X%*%beta
#prediction SEs (logistic scale)
pred_se<-sqrt(diag(X %*% var %*% t(X)))
mu_with_rand<-function(pred,randVar,ginv=function(x){exp(x)/(1+exp(x))}){
  f<-function(x,m,v){ginv(x)*dnorm(x,m,sqrt(v))}
  integrate(f,lower=pred-4*sqrt(randVar),upper=pred+4*sqrt(randVar),m=pred,v=randVar)$value
}
#random variance
totVar<-sum(as.numeric(VarCorr(inde.son4)))
#predictions
preds<-data.frame(helpMeanAge=c(1:6),data_pred=rep(NA,6),data_upper=rep(NA,6),data_lower=rep(NA,6))
for(i in 1:6){
  preds$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds$data_upper[i]<-mu_with_rand(pred[i]+1*pred_se[i],totVar)
  preds$data_lower[i]<-mu_with_rand(pred[i]-1*pred_se[i],totVar)
}


#summarise age raw data 
offspring<-offspring %>%
  mutate(prop.sons=helpSonNum/helpNum)

survDat<-offspring %>%
  dplyr::select(independent,Mage,Bage,Sage,lifespanB,
                lifespanS,lifespanB,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio,dad_soc,prop.sons,helpMinAge,helpMaxAge)
survDat<-na.omit(survDat)

survDat1 <- survDat%>%
  filter(helpB==1)%>%
  group_by(helpMeanAge,helpSonB)%>%
  summarise(num=n(),surv=mean(independent))
preds<-left_join(survDat1,preds)

sample<-survDat1 %>%
  group_by(helpMeanAge)%>%
  summarise(n=sum(num))%>%
  mutate(helpMeanAge=round(helpMeanAge,3))%>%
  filter(helpMeanAge!=1.333)%>%
  filter(helpMeanAge!=2.667) %>%
  filter(helpMeanAge!=1.25)%>%
  filter(helpMeanAge!=2.75) %>%
  add_row (helpMeanAge=1.25,n=31)%>%
  add_row(helpMeanAge=2.75,n=7)%>%
  mutate(helpMeanAge=ifelse(helpMeanAge==1.667,1.75,helpMeanAge))%>%
  mutate(helpMeanAge=ifelse(helpMeanAge==2.333,2.25,helpMeanAge))%>%
  mutate(helpMeanAge=ifelse(helpMeanAge==3.333,3.25,helpMeanAge))
  
  

preds<-left_join(preds,sample, by="helpMeanAge")

#plot
ggplot(data=preds,aes(x=helpMinAge,y=surv))+
  geom_point(aes(x=helpMeanAge,y=surv,size=num,shape=as.factor(helpSonB)))+
  geom_line(data=na.omit(preds),aes(x=helpMeanAge,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=helpMeanAge,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  #coord_cartesian(ylim=c(0.2,0.6))+
  labs(x="Mean Helper Age",y="Survival to Independence")+
  guides(size="none",shape="none")+
  geom_text(data=sample,aes(x=helpMeanAge,y=1.03,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:6),labels=c(1:6))+
  scale_y_continuous(breaks=c(0.00001,0.25,0.5,0.75,1),labels=c(0,0.25,0.5,0.75,1))+
  theme_minimal()



##########
#graph - Mage
summary(inde.son4) 

survDat<-offspring %>%
  dplyr::select(independent,Mage,Bage,Sage,lifespanB,
                lifespanS,lifespanB,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio,dad_soc)
survDat<-na.omit(survDat)

X<-cbind(1,
         c(1:9),
         mean(survDat$lifespanM),
         mean(survDat$EPb),
         mean(survDat$Jincube),
         mean(survDat$helpSonB),
         mean(survDat$helpUnrelateB),
         mean(subset(survDat$Bage,survDat$EPb==1,Bage)),
         mean(subset(survDat$Sage,survDat$EPb==1,Sage)),
         mean(subset(survDat$Bage,survDat$EPb==0,Bage)),
         mean(subset(survDat$lifespanB,survDat$EPb==1,Bage)),
         mean(subset(survDat$lifespanS,survDat$EPb==1,Sage)),
         mean(subset(survDat$lifespanB,survDat$EPb==0,Bage)),
         mean(subset(survDat$helpMeanAge,survDat$helpB==1,helpMeanAge)))
#extract var covar and fixed effect estimates
var<-vcov(inde.son4)
beta<-fixef(inde.son4)
#predictions (logistic scale)
pred<-X%*%beta
#prediction SEs (logistic scale)
pred_se<-sqrt(diag(X %*% var %*% t(X)))
mu_with_rand<-function(pred,randVar,ginv=function(x){exp(x)/(1+exp(x))}){
  f<-function(x,m,v){ginv(x)*dnorm(x,m,sqrt(v))}
  integrate(f,lower=pred-4*sqrt(randVar),upper=pred+4*sqrt(randVar),m=pred,v=randVar)$value
}
#random variance
totVar<-sum(as.numeric(VarCorr(inde.son4)))
#predictions
preds<-data.frame(Mage=c(1:9),data_pred=rep(NA,9),data_upper=rep(NA,9),data_lower=rep(NA,9))
for(i in 1:9){
  preds$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds$data_upper[i]<-mu_with_rand(pred[i]+1*pred_se[i],totVar)
  preds$data_lower[i]<-mu_with_rand(pred[i]-1*pred_se[i],totVar)
}


#summarise age raw data 
survDat<-survDat %>%
  mutate(Mage =ifelse(Mage<7,Mage,6))
survDat1 <- survDat%>%
  group_by(Mage)%>%
  summarise(num=n(),surv=mean(independent))
preds<-left_join(survDat1,preds)

sample<-survDat1 %>%
  group_by(Mage)%>%
  summarise(n=sum(num))
 

#plot
ggplot(data=preds,aes(x=Mage,y=surv))+
  geom_point(aes(x=Mage,y=surv,size=num))+
  geom_line(data=na.omit(preds),aes(x=Mage,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=Mage,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  #coord_cartesian(ylim=c(0.2,0.6))+
  labs(x="Mother Age",y="Survival to Independence")+
  geom_text(data=sample,aes(x=Mage,y=0.55,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:6),labels=c("1","2","3","4","5","6+"))+
  guides(size="none")+
  theme_minimal()

##########
#graph - Maternal lifespan
summary(inde.son4) 

X<-cbind(1,
         mean(survDat$Mage),
         c(1:10),
         0.5484728,
         mean(survDat$Jincube),
         mean(survDat$helpSonB),
         mean(survDat$helpUnrelateB),
         mean(subset(survDat$Bage,survDat$EPb==1,Bage)),
         mean(subset(survDat$Sage,survDat$EPb==1,Sage)),
         mean(subset(survDat$Bage,survDat$EPb==0,Bage)),
         mean(subset(survDat$lifespanB,survDat$EPb==1,Bage)),
         mean(subset(survDat$lifespanS,survDat$EPb==1,Sage)),
         mean(subset(survDat$lifespanB,survDat$EPb==0,Bage)),
         mean(subset(survDat$helpMeanAge,survDat$helpB==1,helpMeanAge)))
#extract var covar and fixed effect estimates
var<-vcov(inde.son4)
beta<-fixef(inde.son4)
pred<-X%*%beta
#prediction SEs (logistic scale)
pred_se<-sqrt(diag(X %*% var %*% t(X)))
mu_with_rand<-function(pred,randVar,ginv=function(x){exp(x)/(1+exp(x))}){
  f<-function(x,m,v){ginv(x)*dnorm(x,m,sqrt(v))}
  integrate(f,lower=pred-4*sqrt(randVar),upper=pred+4*sqrt(randVar),m=pred,v=randVar)$value
}
#random variance
totVar<-sum(as.numeric(VarCorr(recruitHA1)))
#predictions
preds<-data.frame(lifespanM=c(1:10),data_pred=rep(NA,10),data_upper=rep(NA,10),data_lower=rep(NA,10))
for(i in 1:10){
  preds$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds$data_upper[i]<-mu_with_rand(pred[i]+1*pred_se[i],totVar)
  preds$data_lower[i]<-mu_with_rand(pred[i]-1*pred_se[i],totVar)
}



#summarise age raw data 
survDat1 <- survDat%>%
  group_by(lifespanM)%>%
  summarise(num=n(),surv=mean(independent))

sample<-survDat1 %>%
  group_by(lifespanM)%>%
  summarise(n=sum(num))

preds<-left_join(survDat1,preds)

#plot
ggplot(data=preds,aes(x=lifespanM,y=surv))+
  geom_point(aes(x=lifespanM,y=surv,size=num))+
  geom_line(data=na.omit(preds),aes(x=lifespanM,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=lifespanM,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  #coord_cartesian(ylim=c(0.2,0.6))+
  labs(x="Mother Lifespan",y="Survival to Independence")+
  guides(size="none")+
  geom_text(data=sample,aes(x=lifespanM,y=0.7,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:10),labels=c(1:10))+
  theme_minimal()





