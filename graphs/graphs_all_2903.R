#graphs to be put in the MS
#cleaned up and organised
#graphs are altered from their original forms which are perserved in:
#graph_MM_indefathers,graph_MM_inde, graph_MM_recruit

###########
#models
#we'll use binary helper presence models for simplicity of making predictions

####
#survival to independence
inde.son4<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son4)
offspring<-mutate(offspring, helpUnrelateB=ifelse(helpUnrelateNum>0,1,0))
survDat<-offspring %>%
  dplyr::select(independent,Mage,Bage,Sage,lifespanB,
                lifespanS,lifespanB,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio,dad_soc)
survDat<-na.omit(survDat)

####
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

######################################################################################
#maternal
##########

#independence
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
         mean(survDat$helpMeanAge))

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
  coord_cartesian(ylim=c(0.15,0.7))+
  labs(x="Maternal Age",y="Survival to Independence")+
  geom_text(data=sample,aes(x=Mage,y=0.7,label=n),size=3)+
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
  coord_cartesian(ylim=c(0.15,0.7))+
  labs(x="Maternal Lifespan",y="Survival to Independence")+
  guides(size="none")+
  geom_text(data=sample,aes(x=lifespanM,y=0.7,label=n),size=3)+
  scale_x_continuous(breaks=c(1:10),labels=c(1:10))+
  theme_minimal()


#####
#recruitment - maternal age
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
         mean(recruitDat$helpMeanAge))

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
  coord_cartesian(ylim=c(0.15,0.7))+
  geom_text(data=sample,aes(x=Mage,y=0.7,label=n),size=3)+
  scale_x_continuous(breaks=c(1:6),labels=c("1","2","3","4","5","6+"))+
  labs(x="Maternal Age",y="Probability of Recruitment")+
  guides(size="none")+
  theme_minimal()

#########################################
#Maternal Lifespan

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
         mean(recruitDat$helpMeanAge))

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
  coord_cartesian(ylim=c(0.15,0.7))+
  labs(x="Maternal Lifespan",y="Probability of Recruitment")+
  geom_text(data=sample,aes(x=lifespanM,y=0.7,label=n),size=3)+
  scale_x_continuous(breaks=c(1:10),labels=c(1:10))+
  guides(size="none")+
  theme_minimal()


#####################################################################################
#helpers
#######

#independence
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
survDat1 <- survDat%>%
  filter(helpB==1)%>%
  group_by(helpMeanAge)%>%
  summarise(num=n(),surv=mean(independent))
preds<-left_join(survDat1,preds,by="helpMeanAge")

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
ggplot(data=preds,aes(x=helpMeanAge,y=surv))+
  geom_point(aes(x=helpMeanAge,y=surv,size=num))+
  geom_line(data=na.omit(preds),aes(x=helpMeanAge,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=helpMeanAge,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  coord_cartesian(ylim=c(0,1))+
  labs(x="Mean Helper Age",y="Survival to Independence")+
  guides(size="none",shape="none")+
  geom_text(data=sample,aes(x=helpMeanAge,y=1.03,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:6),labels=c(1:6))+
  #scale_y_continuous(breaks=c(0.00001,0.25,0.5,0.75,1),labels=c(0,0.25,0.5,0.75,1))+
  scale_y_continuous(breaks=c(0,0.25,0.50,0.75,1.00),labels=c(0,0.25,0.50,0.75,1.00))+
  theme_minimal()

#########
#recruitment
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
  group_by(helpMeanAge)%>%
  summarise(num=n(),recruit=mean(recruit,na.rm=TRUE))
preds<-left_join(recruitDat1,preds,by="helpMeanAge")

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
  geom_point(aes(x=helpMeanAge,y=recruit,size=num))+
  geom_line(data=na.omit(preds),aes(x=helpMeanAge,y=data_pred))+
  geom_ribbon(data=na.omit(preds),aes(x=helpMeanAge,ymin=data_lower,ymax=data_upper),alpha=0.1)+
  #coord_cartesian(ylim=c(0.2,0.6))+
  labs(x="Mean Helper Age",y="Probability of Recruitment")+
  guides(size="none",shape="none")+
  geom_text(data=sample,aes(x=helpMeanAge,y=1.03,label=n),size=2.5)+
  scale_x_continuous(breaks=c(1:6),labels=c(1:6))+
  scale_y_continuous(breaks=c(0,0.25,0.50,0.75,1.00),labels=c(0,0.25,0.50,0.75,1.00))+
  theme_minimal()


#########################################################################
#fathers - survival to inde

indeDat<-offspring %>%
  dplyr::select(independent,Mage,Bage,Sage,lifespanB,
                lifespanS,lifespanB,lifespanM,Jincube,EPb,WPb,helpMeanAge,helpB,helpSonB,helpUnrelateB,
                cohort, mum, dad_bio,dad_soc)
indeDat<-na.omit(indeDat)

##############
#raw means

indeDat<-indeDat %>%
  mutate(BageC=ifelse(Bage>8,8,Bage))%>%
  mutate(SageC=ifelse(Sage>8,8,Sage))

#summarise age raw data 
indeDatWPB <- indeDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(independent))%>%
  mutate(parent=as.factor("WPB"))
indeDatEPS <- indeDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(num=n(),surv=mean(independent))%>%
  mutate(parent=as.factor("EPS"))%>%
  rename(BageC=SageC)
indeDatEPB <- indeDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(independent))%>%
  mutate(parent=as.factor("EPB"))


########################################################################
#WP Bage
##############################
#manually create matrix of all the fixed effects at their desired values (1:10 for Bage, means for others, 0 and 1 for EPb/WPb)
#make sure these are in the same order as in the model results summary
summary(inde.son4)
X<-cbind(1,
         mean(indeDat$Mage),
         mean(indeDat$lifespanM),
         0,
         mean(indeDat$Jincube),
         mean(indeDat$helpSonB),
         mean(indeDat$helpUnrelateB),
         0,
         0,
         c(1:8)*1,
         0,
         0,
         mean(indeDat$lifespanB),
         mean(indeDat$helpMeanAge))
#extract var covar and fixed effect estimates
var<-vcov(inde.son4)
beta<-fixef(inde.son4)
#predictions (logistic scale)
#this gives you predictions for each Bage (need to convert by logit inverse still)
pred<-X%*%beta
#prediction SEs (logistic scale)
pred_se<-sqrt(diag(X %*% var %*% t(X)))


mu_with_rand<-function(pred,randVar,ginv=function(x){exp(x)/(1+exp(x))}){
  f<-function(x,m,v){ginv(x)*dnorm(x,m,sqrt(v))}
  integrate(f,lower=pred-4*sqrt(randVar),upper=pred+4*sqrt(randVar),m=pred,v=randVar)$value
}

# check
invlogit<-function(x){exp(x)/(1+exp(x))}
x<-rnorm(10000,-2,1)
mean(invlogit(x))
mu_with_rand(-2,1)

#random variance
totVar<-sum(as.numeric(VarCorr(inde.son4)))

preds<-data.frame(Bage=c(1:8),data_pred=rep(NA,8),data_upper=rep(NA,8),data_lower=rep(NA,8))


for(i in 1:8){
  preds$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds$data_upper[i]<-mu_with_rand(pred[i]+1*pred_se[i],totVar)
  preds$data_lower[i]<-mu_with_rand(pred[i]-1*pred_se[i],totVar)
}
preds<-left_join(preds,indeDatWPB,by=c("Bage"="BageC"))

###############################################################################################
##EP Bage
##############################
summary(inde.son4)
X<-cbind(1,
         mean(indeDat$Mage),
         mean(indeDat$lifespanM),
         1,
         mean(indeDat$Jincube),
         mean(indeDat$helpSonB),
         mean(indeDat$helpUnrelateB),
         c(1:8)*1,
         0,
         0,
         0,
         0,
         mean(indeDat$lifespanB),
         mean(indeDat$helpMeanAge))
#predictions (logistic scale)
pred1<-X%*%beta
#prediction SEs (logistic scale)
pred_se1<-sqrt(diag(X %*% var %*% t(X)))

mu_with_rand<-function(pred1,randVar,ginv=function(x){exp(x)/(1+exp(x))}){
  f<-function(x,m,v){ginv(x)*dnorm(x,m,sqrt(v))}
  integrate(f,lower=pred1-4*sqrt(randVar),upper=pred1+4*sqrt(randVar),m=pred1,v=randVar)$value
}

##############
preds1<-data.frame(Bage=c(1:8),data_pred=rep(NA,8),data_upper=rep(NA,8),data_lower=rep(NA,8))
for(i in 1:8){
  preds1$data_pred[i]<-mu_with_rand(pred1[i],totVar)
  preds1$data_upper[i]<-mu_with_rand(pred1[i]+0.5*pred_se1[i],totVar)
  preds1$data_lower[i]<-mu_with_rand(pred1[i]-0.5*pred_se1[i],totVar)
}
preds1<-left_join(preds1,indeDatEPB,by=c("Bage"="BageC"))

###############################################################################################
##EP Sage
##############################
summary(inde.son4)
X<-cbind(1,
         mean(indeDat$Mage),
         mean(indeDat$lifespanM),
         1,
         mean(indeDat$Jincube),
         mean(indeDat$helpSonB),
         mean(indeDat$helpUnrelateB),
         0,
         c(1:8)*1,
         0,
         0,
         0,
         mean(indeDat$lifespanB),
         mean(indeDat$helpMeanAge))
#predictions (logistic scale)
pred2<-X%*%beta
#prediction SEs (logistic scale)
pred_se2<-sqrt(diag(X %*% var %*% t(X)))

#########################
preds2<-data.frame(Bage=c(1:8),data_pred=rep(NA,8),data_upper=rep(NA,8),data_lower=rep(NA,8))
for(i in 1:8){
  preds2$data_pred[i]<-mu_with_rand(pred2[i],totVar)
  preds2$data_upper[i]<-mu_with_rand(pred2[i]+0.5*pred_se2[i],totVar)
  preds2$data_lower[i]<-mu_with_rand(pred2[i]-0.5*pred_se2[i],totVar)
}
preds2<-left_join(preds2,indeDatEPS,by=c("Bage"="BageC"))

########################################################
#combine the 3 preds tables
survPred<-merge(preds,preds1,all=TRUE)
survPred<-merge(survPred,preds2,all=TRUE)

sample<-survPred %>%
  group_by(Bage)%>%
  summarise(n=sum(num))
survPred<-survPred %>%
  rename(Father=parent)

survPred<-survPred %>%
  mutate(Father=ifelse(Father=="WPB","Within-Pair",ifelse(Father=="EPB","Extra-Pair Genetic","Extra-Pair Social")))

#plot
ggplot(data=survPred,aes(x=Bage,y=data_pred))+
  geom_point(mapping=aes(x=Bage,surv,shape=Father,colour=Father,size=num))+
  geom_line(mapping=aes(x=Bage,y=data_pred,colour=Father,linetype=Father),size=1)+
  geom_ribbon(mapping=aes(x=Bage,ymax=data_upper,ymin=data_lower,fill=Father),color=NA,alpha=0.1)+
  labs(x="Paternal Age",y="Chick Survival")+
  geom_text(data=sample,mapping=(aes(x=Bage,y=0.65,label=n)))+
  coord_cartesian(xlim=c(1:8),ylim=c(0.3,0.65))+
  guides(size="none")+
  theme_minimal()


########################






