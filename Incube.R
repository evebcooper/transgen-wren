

#inde- no incube
indeD<-glmer(independent~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+
               (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
             data=offspring, family="binomial",control=glmerControl(optimizer ="bobyqa"))
summary(indeD)
#postive effect of WP Bage becomes NS (p=0.08) when you exclude Jincube



weightD<-lmer(weight~Mage+EPb+help.cat+Bage:EPb+Sage:EPb+Bage:WPb+weight.age+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=subset(offspring,sire!="WG"),control=lmerControl(optimizer ="bobyqa"))
summary(weightD)
#negative effect of EP Bage is still significant (p=0.03, rather than p=0.01)



#############
#lifespan


#males - help, with incube
life.m<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+help.cat+Jincube+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.m)
#positive effect of WP lifespan, no effect of incube

#males - help, without incube
life.m.i<-glmmTMB(lifespan~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                  EPb+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=subset(offspring_long,sex=="M"))
summary(life.m.i)
#still positive, slightly less signficant
#it makes sense to include incube in lifespan to account for 1 year olds that are born later in the year being younger

#base
life<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life)
#nothing significant

#including incube
life1<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life1)
#nothing significant


#include helpers
life2<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life2)
#nothing significant (including helpers)

#include incube and helpers
life3<-glmmTMB(lifespan~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+help.cat+Jincube+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom2(),data=offspring_long)
summary(life3)
#nothing significant
#######
#REL vs. mean 
lifeRM<-glmmTMB(lifespan~MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=offspring_long)
summary(lifeRM)
#nothing significant

#REL vs. mean - incube and helpers
lifeRM1<-glmmTMB(lifespan~MageREL+MageM+EPb+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+
                  Jincube+help.cat+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                family=nbinom2(),data=offspring_long)
summary(lifeRM1)
#nothing significant

###############
#LRS (adults)

#males-base model
lrs.m1<-glmmTMB(LRStot~Mage+EPb+Bage:EPb+Sage:EPb+Bage:WPb+Jincube+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc) ,
                ziformula = ~.,
                data=subset(offspring_lrs,sex=="M"),family=nbinom1)
summary(lrs.m1)
#boderline positive (p=0.06) effect of WP Bage

