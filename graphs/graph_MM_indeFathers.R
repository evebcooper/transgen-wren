#survival to independence
inde.son4<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                   EPb+Jincube+helpMeanAge:helpB+helpSonB+helpUnrelateB+
                   (1|mum)+(1|cohort)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
                 data=offspring, family="binomial")
summary(inde.son4)
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
  geom_point(mapping=aes(x=Bage,surv,shape=Father,colour=Father))+
  geom_line(mapping=aes(x=Bage,y=data_pred,colour=Father,linetype=Father),size=1)+
  geom_ribbon(mapping=aes(x=Bage,ymax=data_upper,ymin=data_lower,fill=Father),color=NA,alpha=0.1)+
  labs(x="Father Age",y="Chick Survival")+
  geom_text(data=sample,mapping=(aes(x=Bage,y=0.65,label=n)))+
  coord_cartesian(xlim=c(1:8),ylim=c(0.3,0.65))+
  #guides(shape="none",colour="none",fill="none",size="none")+
  theme_bw()





table(indeDat$EPb)









