#run some GAMMs to look at the shape of offspring survival for each parent

library(mgcv)
library(gamm4)
study_born$cohort<-as.factor(study_born$cohort)
study_born$mum<-as.factor(study_born$mum)
study_born$dad_soc<-as.factor(study_born$dad_soc)
study_born$dad_bio<-as.factor(study_born$dad_bio)

fledgeEP.gamm<-gamm4(fledged~s(Mage,k=9)+s(Sage)+s(Bage),random=~(1|cohort),
                data=study_born,subset=study_born$EP=="yes",REML=TRUE,binomial(link="logit"))
summary(fledgeEP.gamm$gam)
summary(fledgeEP.gamm$mer)
gam.check(fledgeEP.gamm)


fledgeEP.gam<-gam(fledged~s(Mage,k=9)+s(Sage)+s(Jincube)+s(Bage)+s(mum,bs="re")+s(dad_soc,bs="re")+s(dad_bio,bs="re"),
                     data=study_born,subset=study_born$EP=="yes",REML=TRUE,binomial(link="logit"))
summary(fledgeEP.gam)
gam.check(fledgeEP.gam)
plot(fledgeEP.gam,shade=TRUE,trans=plogis,shift=coef(fledgeEP.gam)[1],ylim=c(0,1),
     xlab="",ylab="Probability",main="Mother's age",select=1)
plot(fledgeEP.gam,shade=TRUE,trans=plogis,shift=coef(fledgeEP.gam)[1],ylim=c(0.5,0.9),
     xlab="",ylab="Probability",main="Social Father age",select=2)
plot(fledgeEP.gam,shade=TRUE,trans=plogis,shift=coef(fledgeEP.gam)[1],ylim=c(0,1),
     xlab="",ylab="Probability",main="Bio Father age",select=4)
