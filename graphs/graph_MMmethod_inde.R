#survival to independence
surv<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
              EPb+Jincube+weight.res+
              (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
            data=offspring, family="binomial")
summary(surv)

##############
#raw means

#dataframe
indeDat<-offspring %>%
  dplyr::select(Mage,MageC,Bage,Sage,BageC,SageC, lifespanB,lifespanS,lifespanB,lifespanM,Jincube,weight.res,EPb,
         cohort, mum, dad_bio,dad_soc,WPb,independent,sex)
indeDat<-na.omit(indeDat)

#summarise age raw data 
indeDatWPB <- indeDat%>%
  filter(EPb==0)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(independent))
indeDatEPS <- indeDat%>%
  filter(EPb==1)%>%
  group_by(SageC)%>%
  summarise(num=n(),surv=mean(independent))
indeDatEPB <- indeDat%>%
  filter(EPb==1)%>%
  group_by(BageC)%>%
  summarise(num=n(),surv=mean(independent))
indeDatmum <- indeDat%>%
  group_by(MageC)%>%
  summarise(num=n(),surv=mean(independent))


########################################################################
#WP Bage
##############################
#manually create matrix of all the fixed effects at their desired values (1:10 for Bage, means for others, 0 and 1 for EPb/WPb)
#make sure these are in the same order as in the model results summary

X<-cbind(1,mean(indeDat$Mage),
         mean(indeDat$lifespanM),
         0,
         mean(indeDat$Jincube),
         mean(indeDat$weight.res),
         0,
         0,
         c(1:10)*1,
         0,
         0,
         mean(indeDat$lifespanB))
         


#extract var covar and fixed effect estimates
var<-vcov(surv)
beta<-fixef(surv)

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
totVar<-sum(as.numeric(VarCorr(surv)))

preds<-data.frame(Bage=c(1:10),data_pred=rep(NA,10),data_upper=rep(NA,10),data_lower=rep(NA,10),parent="WPb")


for(i in 1:10){
  preds$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds$data_upper[i]<-mu_with_rand(pred[i]+1.96*pred_se[i],totVar)
  preds$data_lower[i]<-mu_with_rand(pred[i]-1.96*pred_se[i],totVar)
}

preds<-left_join(preds,indeDatWPB,by=c("Bage"="BageC"))

plot(1:10,preds$data_pred,type='l',ylim=c(0,1))
lines(1:10,preds$data_lower,lty="dashed")
lines(1:10,preds$data_upper,lty="dashed")


lines(1:10,invlogit(pred),col="red",lty="dotted")


X %*% var %*% t(X)



###############################################################################################
##EP Bage
##############################
X<-cbind(1,mean(indeDat$Mage),
         mean(indeDat$lifespanM),
         1,
         mean(indeDat$Jincube),
         mean(indeDat$weight.res),
         c(1:10)*1,
         mean(indeDat$Sage)*1,
         0,
         mean(indeDat$lifespanB),
         mean(indeDat$lifespanS),
         0)
#predictions (logistic scale)
pred<-X%*%beta
#prediction SEs (logistic scale)
pred_se<-sqrt(diag(X %*% var %*% t(X)))
##############
preds1<-data.frame(Bage=c(1:10),data_pred=rep(NA,10),data_upper=rep(NA,10),data_lower=rep(NA,10),parent="EPb")
for(i in 1:10){
  preds1$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds1$data_upper[i]<-mu_with_rand(pred[i]+1.96*pred_se1[i],totVar)
  preds1$data_lower[i]<-mu_with_rand(pred[i]-1.96*pred_se1[i],totVar)
}
preds1<-left_join(preds1,indeDatEPB,by=c("Bage"="BageC"))

###############################################################################################
##EP Sage
##############################

X<-cbind(1,mean(indeDat$Mage),
         mean(indeDat$lifespanM),
         1,
         mean(indeDat$Jincube),
         mean(indeDat$weight.res),
         mean(indeDat$Bage)*1,
         c(1:10)*1,
         0,
         mean(indeDat$lifespanB),
         mean(indeDat$lifespanS),
         0)
#predictions (logistic scale)
pred<-X%*%beta
#prediction SEs (logistic scale)
pred_se<-sqrt(diag(X %*% var %*% t(X)))

#########################
preds2<-data.frame(Bage=c(1:10),data_pred=rep(NA,10),data_upper=rep(NA,10),data_lower=rep(NA,10),parent="EPs")
for(i in 1:10){
  preds2$data_pred[i]<-mu_with_rand(pred[i],totVar)
  preds2$data_upper[i]<-mu_with_rand(pred[i]+1.96*pred_se1[i],totVar)
  preds2$data_lower[i]<-mu_with_rand(pred[i]-1.96*pred_se1[i],totVar)
}
preds2<-left_join(preds2,indeDatEPS,by=c("Bage"="SageC"))

########################################################
#combine the 3 preds tables
survPred<-merge(preds,preds1,all=TRUE)
survPred<-merge(survPred,preds2,all=TRUE)


#plot
ggplot(data=survPred,aes(x=Bage,y=data_pred,colour=parent))+
  geom_point(mapping=aes(x=Bage,surv,shape=parent))+
  geom_line(mapping=aes(x=Bage,y=data_pred),size=1)+
  geom_ribbon(mapping=aes(x=Bage,ymax=data_upper,ymin=data_lower,fill=parent),color=NA,alpha=0.1)+
  labs(x="Parent Age",y="Chick Survival")+
  coord_cartesian(xlim=c(1:8),ylim=c(0.3,0.65))+
  theme_bw()












