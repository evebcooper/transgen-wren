###############################################################################
#recruitment
male.recruit<- offspring %>%
  filter(sex=="M") %>%
  mutate(recruit=ifelse(lifespanSep>0,1,0))%>%
  filter(fate!="Dispersedoutside")%>%
  filter(fate!="Unknown")

recruitHA1<-glmer(recruit~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                    EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                    (1|cohort)+(1|mum)+(1|dad_bio),
                  data=male.recruit, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(recruitHA1)

recruitDat<-male.recruit %>%
  dplyr::select(recruit,Mage,Bage,Sage,lifespanB,lifespanS,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio)
recruitDat<-na.omit(recruitDat)

###################################################################
#helper mean age
#manually create matrix of all the fixed effects at their desired values (1:6 for helpMeanAge, means for others, 1 for helpB)
#make sure these are in the same order as in the model results summary
X<-cbind(1,
         mean(recruitDat$Mage),
         mean(recruitDat$lifespanM),
         0.5484728,
         mean(recruitDat$Jincube),
         mean(recruitDat$helpSonB),
         mean(recruitDat$helpUnrelateB),
         mean(subset(recruitDat$Bage,recruitDat$EPb==1,Bage)),
         mean(subset(recruitDat$Sage,recruitDat$EPb==1,Sage)),
         mean(subset(recruitDat$Bage,recruitDat$EPb==0,Bage)),
         mean(subset(recruitDat$lifespanB,recruitDat$EPb==1,Bage)),
         mean(subset(recruitDat$lifespanS,recruitDat$EPb==1,Sage)),
         mean(subset(recruitDat$lifespanB,recruitDat$EPb==0,Bage)),
         c(1:6))
#extract var covar and fixed effect estimates
var<-vcov(recruitHA1)
beta<-fixef(recruitHA1)
#predictions (logistic scale)
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
preds<-data.frame(helpMeanAge=c(1:6),data_pred=rep(NA,6),data_upper=rep(NA,6),data_lower=rep(NA,6))
for(i in 1:6){
  preds$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds$data_upper[i]<-mu_with_rand(pred[i]+1*pred_se[i],totVar)
  preds$data_lower[i]<-mu_with_rand(pred[i]-1*pred_se[i],totVar)
}


#summarise  raw data 
recruitDat1 <- recruitDat%>%
  filter(helpB==1)%>%
  group_by(helpMeanAge,helpSonB)%>%
  summarise(num=n(),recruit=mean(recruit,na.rm=TRUE))
preds<-left_join(recruitDat1,preds)

sample<-recruitDat1 %>%
group_by(helpMeanAge)%>%
  summarise(n=sum(num))%>%
  mutate(helpMeanAge=round(helpMeanAge,3))%>%
  filter(helpMeanAge!=1.333)%>%
  filter(helpMeanAge!=2.667) %>%
  filter(helpMeanAge!=1.25)%>%
  filter(helpMeanAge!=2.75) %>%
  add_row (helpMeanAge=1.25,n=20)%>%
  add_row(helpMeanAge=2.75,n=3)%>%
  mutate(helpMeanAge=ifelse(helpMeanAge==1.667,1.75,helpMeanAge))%>%
  mutate(helpMeanAge=ifelse(helpMeanAge==2.333,2.25,helpMeanAge))
  
  
  

preds<-left_join(preds,sample, by="helpMeanAge")

ggplot(data=preds,aes(x=helpMeanAge,y=recruit))+
  geom_point(aes(x=helpMeanAge,y=recruit,size=num,shape=as.factor(helpSonB)))+
  geom_line(data=na.omit(preds),aes(x=helpMeanAge,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=helpMeanAge,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  #coord_cartesian(ylim=c(0.2,0.6))+
  labs(x="Mean Helper Age",y="Probability of Recruitment")+
  guides(size="none",shape="none")+
  geom_text(data=sample,aes(x=helpMeanAge,y=1.03,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:6),labels=c(1:6))+
  scale_y_continuous(breaks=c(0,0.25,0.50,0.75,1.00),labels=c(0,0.25,0.50,0.75,1.00))+
  theme_minimal()
  
#########################################
#Mother Age

recruitDat<-male.recruit %>%
  dplyr::select(recruit,Mage,Bage,Sage,lifespanB,lifespanS,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio)
recruitDat<-na.omit(recruitDat)


X<-cbind(1,
         c(1:9),
         mean(recruitDat$lifespanM),
         0.5484728,
         mean(recruitDat$Jincube),
         mean(recruitDat$helpSonB),
         mean(recruitDat$helpUnrelateB),
         mean(subset(recruitDat$Bage,recruitDat$EPb==1,Bage)),
         mean(subset(recruitDat$Sage,recruitDat$EPb==1,Sage)),
         mean(subset(recruitDat$Bage,recruitDat$EPb==0,Bage)),
         mean(subset(recruitDat$lifespanB,recruitDat$EPb==1,Bage)),
         mean(subset(recruitDat$lifespanS,recruitDat$EPb==1,Sage)),
         mean(subset(recruitDat$lifespanB,recruitDat$EPb==0,Bage)),
         mean(subset(recruitDat$helpMeanAge,recruitDat$helpB==1,helpMeanAge)))
#extract var covar and fixed effect estimates
var<-vcov(recruitHA1)
beta<-fixef(recruitHA1)
#predictions (logistic scale)
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
preds<-data.frame(Mage=c(1:9),data_pred=rep(NA,9),data_upper=rep(NA,9),data_lower=rep(NA,9))
for(i in 1:9){
  preds$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds$data_upper[i]<-mu_with_rand(pred[i]+1*pred_se[i],totVar)
  preds$data_lower[i]<-mu_with_rand(pred[i]-1*pred_se[i],totVar)
}

recruitDat<-recruitDat %>%
  mutate(Mage=ifelse(Mage<7,Mage,6))
  
recruitDat1 <- recruitDat%>%
  group_by(Mage)%>%
  summarise(num=n(),recruit=mean(recruit,na.rm=TRUE))
preds<-left_join(recruitDat1,preds)

sample<-recruitDat1 %>%
  group_by(Mage)%>%
  summarise(n=sum(num))

ggplot(data=preds,aes(x=Mage,y=recruit))+
  geom_point(aes(x=Mage,y=recruit,size=num))+
  geom_line(data=na.omit(preds),aes(x=Mage,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=Mage,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  #coord_cartesian(ylim=c(0.2,0.6))+
  geom_text(data=sample,aes(x=Mage,y=0.4,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:6),labels=c("1","2","3","4","5","6+"))+
  labs(x="Mother Age",y="Probability of Recruitment")+
  guides(size="none")+
  theme_minimal()

#########################################
#Mother Lifespan

X<-cbind(1,
         mean(recruitDat$Mage),
         c(1:10),
         0.5484728,
         mean(recruitDat$Jincube),
         mean(recruitDat$helpSonB),
         mean(recruitDat$helpUnrelateB),
         mean(subset(recruitDat$Bage,recruitDat$EPb==1,Bage)),
         mean(subset(recruitDat$Sage,recruitDat$EPb==1,Sage)),
         mean(subset(recruitDat$Bage,recruitDat$EPb==0,Bage)),
         mean(subset(recruitDat$lifespanB,recruitDat$EPb==1,Bage)),
         mean(subset(recruitDat$lifespanS,recruitDat$EPb==1,Sage)),
         mean(subset(recruitDat$lifespanB,recruitDat$EPb==0,Bage)),
         mean(subset(recruitDat$helpMeanAge,recruitDat$helpB==1,helpMeanAge)))
#extract var covar and fixed effect estimates
var<-vcov(recruitHA1)
beta<-fixef(recruitHA1)
#predictions (logistic scale)
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

recruitDat<-male.recruit %>%
  dplyr::select(recruit,Mage,Bage,Sage,lifespanB,lifespanS,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio)
recruitDat<-na.omit(recruitDat)

recruitDat1 <- recruitDat%>%
  group_by(lifespanM)%>%
  summarise(num=n(),recruit=mean(recruit,na.rm=TRUE))
preds<-left_join(recruitDat1,preds)

sample<-recruitDat1 %>%
  group_by(lifespanM)%>%
  summarise(n=sum(num))

ggplot(data=preds,aes(x=lifespanM,y=recruit))+
  geom_point(aes(x=lifespanM,y=recruit,size=num))+
  geom_line(data=na.omit(preds),aes(x=lifespanM,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=lifespanM,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  #coord_cartesian(ylim=c(0.2,0.6))+
  labs(x="Mother Lifespan",y="Probability of Recruitment")+
  geom_text(data=sample,aes(x=lifespanM,y=0.5,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:10),labels=c(1:10))+
  guides(size="none")+
  theme_minimal()

