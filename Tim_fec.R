#average annual fecundity
#include only individuals that survived to age one (recruited)

offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)%>%
  filter(sex!="U")%>%
  filter(independent==1) %>%
  filter(sire!="WG")

mean(offspring_fec$fec.rate)
var(offspring_fec$fec.rate)


#male
fec.EPm<-glmmTMB(fec.rate~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+EPb+Jincube+help.cat+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,sex=="M"),family=compois(link="log"))
summary(fec.EPm)
#NAs produced

#male - no Jincube
fec.EPm1<-glmmTMB(fec.rate~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+EPb+help.cat+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,sex=="M"),family=compois(link="log"))
summary(fec.EPm1)


#male - increase interations
fec.EPm2<-glmmTMB(fec.rate~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+EPb+help.cat+Jincube+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  ziformula = ~.,
                  data=subset(offspring_fec,sire!="WG"&sex=="M"),family=compois(link="log"),
                  glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))
summary(fec.EPm2)


#male - increase interations, no Jincube
fec.EPm3<-glmmTMB(fec.rate~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+EPb+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  ziformula = ~.,
                  data=subset(offspring_fec,sire!="WG"&sex=="M"),family=compois(link="log"),
                  glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))
summary(fec.EPm3)

#################################
#female

glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))
fec.EPf<-glmmTMB(fec.rate~MageREL+MageM+BageREL:EPb+BageM:EPb+SageREL:EPb+SageM:EPb+BageREL:WPb+BageM:WPb+EPb+Jincube+help.cat+
                   (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 ziformula = ~.,
                 data=subset(offspring_fec,sex=="F"),family=compois(link="log"))
summary(fec.EPm)
#NAs produced




