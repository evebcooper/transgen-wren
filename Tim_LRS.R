#LRS

#all study born chicks
#NB this excludes females that immigrated to the study site at age 1
#overall LRS
offspring_lrstot<- offspring %>%
  filter(fate=="Died") 


#male LRS 
lrs.m<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                 ziformula = ~.,
                 data=subset(offspring_lrstot,sex=="M"),family=nbinom1)
summary(lrs.m)
#model not fitting

#male REL and mean
lrs.mREL<-glmmTMB(LRStot~MageREL+MageM+EPb+BageREL:EPb+SageREL:EPb+BageREL:WPb+BageM:EPb+SageM:EPb+BageM:WPb+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
               ziformula = ~.,
               data=subset(offspring_lrstot,sex=="M"),family=nbinom1)
summary(lrs.mREL)
#positive effect of WP father mean age on probability of LRS
#negative effect of EP Bage mean on probability of LRS
#positive effect of EP Bage mean on conditional LRS
#n = 2539 (more than double the data set below)

#female LRS 
lrs.f<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
               ziformula = ~.,
               data=subset(offspring_lrstot,sex=="F"),family=nbinom2)
summary(lrs.f)
#no significant effects


#female REL and mean
lrs.fREL<-glmmTMB(LRStot~MageREL+MageM+EPb+BageREL:EPb+SageREL:EPb+BageREL:WPb+BageM:EPb+SageM:EPb+BageM:WPb+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,sex=="F"),family=nbinom2)
summary(lrs.fREL)
#no effects

###################################
#include Jincube and help.cat

#male LRS 
lrs.m1<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
               ziformula = ~.,
               data=subset(offspring_lrstot,sex=="M"),family=nbinom1)
summary(lrs.m1)
#probability: positive effect of WP bage

#male REL and mean
lrs.mRE1L<-glmmTMB(LRStot~MageREL+MageM+EPb+BageREL:EPb+SageREL:EPb+BageREL:WPb+BageM:EPb+SageM:EPb+BageM:WPb+
                     +Jincube+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,sex=="M"),family=nbinom1)
summary(lrs.mRE1L)
#positive effect of WP father mean age on probability of LRS
#having more than 1 helper very important for conditional success


#female LRS 
lrs.f1<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
               ziformula = ~.,
               data=subset(offspring_lrstot,sex=="F"),family=nbinom2)
summary(lrs.f1)
#no significant effects 


#female REL and mean
lrs.fRE1L<-glmmTMB(LRStot~MageREL+MageM+EPb+BageREL:EPb+SageREL:EPb+BageREL:WPb+BageM:EPb+SageM:EPb+BageM:WPb+
                     +Jincube+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                  ziformula = ~.,
                  data=subset(offspring_lrstot,sex=="F"),family=nbinom2)
summary(lrs.fRE1L)
#no effects (again, not dif. from simpler model)




###################################################
#LRS of all females
#note that even though this dataset is bigger the extra females are unusable 
#since we don't know the parental ages of immigrants

LRSdat <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/LRSdat.csv")

#NB2
lrs.fC<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                  ziformula = ~.,
                  data=subset(LRSdat,sex=="F"),family=nbinom2)
summary(lrs.fC)
#AIC = 1935.4
#n = 1718
#no effects
#NB the sample size is similar to the above models using offspring_lrstot since females that immigrated have unknown Mage/Bage

#truncated nb2
lrs.fC2<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                ziformula = ~.,
                data=subset(LRSdat,sex=="F"),family=truncated_nbinom2)
summary(lrs.fC2)
#AIC = 1934.7
#no effects, similar to above and AIC are very similar


###################################################
#LRS of all to reach independence and age 1
#include Jincube and help.cat

offspring_lrsA<-offspring_lrstot %>%
  filter(lifespan>0)

table(offspring_lrsA$sex,offspring_lrsA$LRStot)

#male LRS 
lrs.m1a<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                ziformula = ~.,
                data=subset(offspring_lrsA,sex=="M"),family=nbinom1)
summary(lrs.m1a)
#probability: positive effect of WP bage (same as total model)

#male REL and mean
lrs.mRE1La<-glmmTMB(LRStot~MageREL+MageM+EPb+BageREL:EPb+SageREL:EPb+BageREL:WPb+BageM:EPb+SageM:EPb+BageM:WPb+
                     +(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                   ziformula = ~.,
                   data=subset(offspring_lrsA,sex=="M"),family=nbinom1)
summary(lrs.mRE1La)
#positive effect of WP father mean age on probability of LRS (same as total model)
#negative effect of EP bage rel on conditional LRS


#female LRS 
lrs.f1a<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                ziformula = ~.,
                data=subset(offspring_lrsA,sex=="F"),family=nbinom2)
summary(lrs.f1a)
#no significant effects 


#female REL and mean
lrs.fRE1La<-glmmTMB(LRStot~MageREL+MageM+EPb+BageREL:EPb+SageREL:EPb+BageREL:WPb+BageM:EPb+SageM:EPb+BageM:WPb+
                     + Jincube+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                   ziformula = ~.,
                   data=subset(offspring_lrsA,sex=="F"),family=nbinom2)
summary(lrs.fRE1La)
#no effects (again, not dif. from simpler model)