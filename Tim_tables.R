library(sjPlot)

##################
#survival to independence
#excluding helper-mother extra-pairings
#excluding son-mother within-pairings

inde.noSon<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+EPb+Jincube+help.cat+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspring,sire!="WG"), family="binomial")
ss <- getME(inde.noSon,c("theta","fixef"))
inde.noSon<- update(inde.noSon,start=ss,control=glmerControl(optCtrl=list(maxfun=8e4)))
summary(inde.noSon)

tab_model(inde.noSon,transform =NULL,show.icc=FALSE,
    pred.labels=c("Intercept","Mother Age","Extra-Group [yes]","Birthdate","Helpers [one]","Helpers [two+]",
                  "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age"
   ),order.terms = c(1,2,7,8,9,3,4,5,6))



##########
#inde rel and mean

indeREL<-glmer(independent~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+EPb+Jincube+help.cat+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspring,sire!="WG"), family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(indeREL)


tab_model(indeREL,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Relative Mother Age","Mean Mother Age","Extra-Group [yes]","Birthdate","Helpers [one]","Helpers [two+]",
                        "Relative Extra-pair Genetic Father Age","Mean Extra-pair Genetic Father Age", 
                        "Relative Extra-pair Social Father Age","Mean Extra-pair Social Father Age", "Relative Within-pair Father Age", 
                        "Mean Within-pair Father Age"),order.terms = c(1,2,3,8,9,10,11,12,13,4,5,6,7))


###########
#weight at banding
#same parameters as above

weight.sep<-lmer(weight~Mage+EPb+Jincube+help.cat+Bage:EPb+Sage:EPb+Bage:WPb+weight.age+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=subset(offspring,sire!="WG"),control=lmerControl(optimizer ="bobyqa"))
summary(weight.sep)


tab_model(weight.sep,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Mother Age","Extra-Group [yes]","Birthdate","Helpers [one]","Helpers [two+]","Age at Weighing",
                        "Extra-pair Genetic Father Age", "Extra-pair Social Father Age", "Within-pair Father Age"
          ),order.terms = c(1,2,8,9,10,3,4,5,6,7))

##############
#weight at banding 
#relative and mean

weight.REL<-lmer(weight~MageREL+MageM+EPb+Jincube+help.cat+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sire!="WG"),control=lmerControl(optimizer ="bobyqa"))
summary(weight.REL)

tab_model(weight.REL,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Relative Mother Age","Mean Mother Age","Extra-Group [yes]","Birthdate","Helpers [one]","Helpers [two+]",
                      "Age at Weighing","Relative Extra-pair Genetic Father Age","Mean Extra-pair Genetic Father Age", 
                      "Relative Extra-pair Social Father Age","Mean Extra-pair Social Father Age", "Relative Within-pair Father Age", 
                      "Mean Within-pair Father Age"),order.terms = c(1,2,3,9,10,11,12,13,14,4,5,6,7,8))

###########################################################
#LRS
#adults only 
offspring_lrs<-offspring %>%
  filter(fate=="Died") %>%
  filter(lifespan>0)


#males-base model
lrs.m1<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+EPb+Jincube+help.cat+
                  (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                ziformula = ~.,
                data=subset(offspring_lrs,sex=="M"),family=nbinom1)
summary(lrs.m1)

tab_model(lrs.m1,transform =NULL,show.icc=FALSE, pred.labels=c("Intercept","Mother Age","Extra-Group [yes]",
                                 "Birthdate","Helpers [one]","Helpers [two+]","Extra-pair Genetic Father Age",
                    "Extra-pair Social Father Age", "Within-pair Father Age"))


#males-rel and mean
lrs.mRE1L<-glmmTMB(LRStot~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+
                     EPb+Jincube+help.cat+(1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                   ziformula = ~.,
                   data=subset(offspring_lrs,sex=="M"),family=nbinom1)
summary(lrs.mRE1L)

tab_model(lrs.mRE1L,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Relative Mother Age","Mean Mother Age","Extra-Group [yes]","Birthdate","Helpers [one]","Helpers [two+]",
          "Relative Extra-pair Genetic Father Age","Mean Extra-pair Genetic Father Age", 
          "Relative Extra-pair Social Father Age","Mean Extra-pair Social Father Age", "Relative Within-pair Father Age", 
          "Mean Within-pair Father Age"))

#female-base model
lrs.f1<-glmmTMB(LRStot~Mage+Bage:EPb+Sage:EPb+Bage:WPb+EPb+Jincube+help.cat+
                  (1|mum)+(1|dad_bio)+(1|dad_soc)+(1|cohort) ,
                ziformula = ~.,
                data=subset(offspring_lrs,sex=="F"),family=nbinom2)
summary(lrs.f1)

tab_model(lrs.f1,transform =NULL,show.icc=FALSE, pred.labels=c("Intercept","Mother Age","Extra-Group [yes]",
        "Birthdate","Helpers [one]","Helpers [two+]","Extra-pair Genetic Father Age",
        "Extra-pair Social Father Age", "Within-pair Father Age"))

#female- rel and mean
glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))
lrs.fRE1L<-glmmTMB(LRStot~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+
                     EPb+Jincube+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                   ziformula = ~.,
                   data=subset(offspring_lrs,sex=="F"),family=nbinom2)
summary(lrs.fRE1L)

tab_model(lrs.fRE1L,transform =NULL,show.icc=FALSE,
          pred.labels=c("Intercept","Relative Mother Age","Mean Mother Age","Extra-Group [yes]","Birthdate","Helpers [one]","Helpers [two+]",
                        "Relative Extra-pair Genetic Father Age","Mean Extra-pair Genetic Father Age", 
                        "Relative Extra-pair Social Father Age","Mean Extra-pair Social Father Age", "Relative Within-pair Father Age", 
                        "Mean Within-pair Father Age"))


#####################################################################
#lifespan
offspring_long<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(sex!="U")%>%
  filter(lifespan>0)%>%
  filter(sire!="WG")

#base
life<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life)


#REL vs. mean 
lifeRM1<-glmmTMB(lifespan~MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+
                     Jincube+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   family=nbinom2(),data=offspring_long)
summary(lifeRM1)






