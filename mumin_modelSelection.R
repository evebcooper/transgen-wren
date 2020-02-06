#model selection for sex-specific effects

library(MuMIn)
library(tidyr)
options(na.action = na.fail)


###########################################
#LRS
offspring_com<- offspring_lrstot %>%
  filter(EP=="yes")%>%
  filter(sex!="U") %>%
  select(ID,LRStot,MageREL,BageREL,SageREL,MageM,BageM,SageM,sex,cohort,mum, dad_bio,dad_soc)
offspring_com<-drop_na(offspring_com) 

LRS.dredge<-glmmTMB(LRStot~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                 +(1|dad_soc),ziformula = ~.,data=offspring_com,family=nbinom1)
getAllTerms(LRS.dredge)
lrd.sel<-dredge(LRS.dredge,beta="sd",fixed=c("cond(MageM)","cond(MageREL)","zi(MageM)","zi(MageREL)",
                                             "cond(SageM)","cond(SageREL)","zi(SageM)","zi(SageREL)",
                                             "cond(BageM)","cond(BageREL)","zi(BageM)","zi(BageREL)"),
                subset=(!"cond(sex)"||{"cond(MageM:sex)"}||{"cond(MageREL:sex)"}||{"cond(BageM:sex)"}||{"cond(BageREL:sex)"}||
                          {"cond(SageM:sex)"}||{"cond(SageREL:sex)"})&&
                  (!"zi(sex)"||{"zi(MageM:sex)"}||{"zi(MageREL:sex)"}||{"zi(BageM:sex)"}||{"zi(BageREL:sex)"}||
                     {"zi(SageM:sex)"}||{"zi(SageREL:sex)"}))
#ran for 20 hours and still not done so stopped process

lrs.sel<-lrd.sel
lrs.sel

#re-run best model using the full dataset
                  

#########################################
#lifespan

offspring_lrsC<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)%>%
  filter(EP=="yes")%>%
  filter(sex!="U") %>%
  select(ID,LRStot,MageREL,BageREL,SageREL,MageM,BageM,SageM,sex,cohort,mum, dad_bio,dad_soc)
offspring_lrsC<-drop_na(offspring_lrsC) 


long.dredge<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+
                 (1|cohort)+(1|mum)+(1|dad_bio) ,
               ziformula = ~.,
               data=offspring_lrsC,family=truncated_nbinom2)
getAllTerms(long.dredge)
long.sel<-dredge(long.dredge,beta="sd",fixed=c("cond(MageM)","cond(MageREL)","zi(MageM)","zi(MageREL)",
                                             "cond(SageM)","cond(SageREL)","zi(SageM)","zi(SageREL)",
                                             "cond(BageM)","cond(BageREL)","zi(BageM)","zi(BageREL)"),
                subset=(!"cond(sex)"||{"cond(MageM:sex)"}||{"cond(MageREL:sex)"}||{"cond(BageM:sex)"}||{"cond(BageREL:sex)"}||
                {"cond(SageM:sex)"}||{"cond(SageREL:sex)"})&&
                  (!"zi(sex)"||{"zi(MageM:sex)"}||{"zi(MageREL:sex)"}||{"zi(BageM:sex)"}||{"zi(BageREL:sex)"}||
                  {"zi(SageM:sex)"}||{"zi(SageREL:sex)"}))
long.sel

###################################
#fecundity rate


offspring_fec <- offspring %>%
  filter(lifespan>0)%>%
  mutate(fec.rate=LRStot/lifespan)

offspring_fecC<- offspring_fec %>%
  filter(EP=="yes")%>%
  filter(sex!="U") %>%
  select(ID,LRStot,MageREL,BageREL,SageREL,MageM,BageM,SageM,sex,cohort,mum, dad_bio,dad_soc)
offspring_lrsC<-drop_na(offspring_lrsC) 


fec.dredge<-glmmTMB(fec.rate~MageREL*sex+BageREL*sex+SageREL*sex+MageM*sex+BageM*sex+SageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)
                  +(1|dad_soc),ziformula = ~.,data=offspring_fecC,family=compois(link="log"))
fec.sel<-dredge(fec.dredge,beta="sd",fixed=c("cond(MageM)","cond(MageREL)","zi(MageM)","zi(MageREL)",
                                               "cond(SageM)","cond(SageREL)","zi(SageM)","zi(SageREL)",
                                               "cond(BageM)","cond(BageREL)","zi(BageM)","zi(BageREL)"),
                 subset=(!"cond(sex)"||{"cond(MageM:sex)"}||{"cond(MageREL:sex)"}||{"cond(BageM:sex)"}||{"cond(BageREL:sex)"}||
                 {"cond(SageM:sex)"}||{"cond(SageREL:sex)"})&&
                   (!"zi(sex)"||{"zi(MageM:sex)"}||{"zi(MageREL:sex)"}||{"zi(BageM:sex)"}||{"zi(BageREL:sex)"}||
                   {"zi(SageM:sex)"}||{"zi(SageREL:sex)"}))
fec.sel



##############
#interpreting interaction effects
#read this
#https://stats.stackexchange.com/questions/381737/seeking-a-post-hoc-test-for-zero-inflated-glmmtmb
#may need to download the lastest development version of glmmTMB from github

library(lsmeans)
#You *should* be able to use lsmeans with glmmTMB objects after running
#source(system.file("other_methods","lsmeans_methods.R",package="glmmTMB"))
#lsmeans(model,pairwise~sex|MageM)





               