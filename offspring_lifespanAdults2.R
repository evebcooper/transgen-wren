#investigating the differences in sex-specific and sex unified lifespan models
options(na.action=na.omit)

offspring_lrs1<- offspring %>%
  filter(fate=="Died")%>%
  filter(independent==1)

table(offspring_lrs1$lifespan)
#problem: only 13 are 0, and so this should not be a zero-inflated model.....

#males
tweedielm<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+MageM+BageM+ 
                     (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                   ziformula = ~.,data=subset(offspring_lrs1,EP=="yes"&sex=="M"),family=tweedie(link="log"))
#time 9.41 - 
summary(tweedielm)


#females
tpoislf<-glmmTMB(lifespan~MageREL+BageREL+SageREL+SageM+(1|dad_soc)+ MageM+BageM+ 
                   (1|cohort)+(1|mum)+(1|dad_bio) ,
                 ziformula = ~.,
                 data=subset(offspring_lrs1,EP=="yes"&sex=="F"),family=truncated_poisson(link="log"))
summary(tpoislf)

#differences in conditional:
#sageREL (fem pos NS, males neg NS)
#Sage M (fem NS, males pos sig)
#differences in conditional
#MageREL (fem pos sig, male pos NS)
#BageREL males neg NS, fem neg sig
#sageM NS but in different directions

#since sex interaction model does not pick any of this up, and has fitting issues we'll try to seperate the models
summary(tNB2lx)


##############################
#probability of survival to 1

offspring.mat<- offspring %>%
  filter(independent==1) %>%
  filter(fate=="Died")%>%
  mutate(mature=ifelse(lifespan>0,1,0))%>%
  filter(sex!="U")


mat.sex<-glmer(mature~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
           family=binomial,data=subset(offspring.mat,EP=="yes"))
ss <- getME(mat.sex,c("theta","fixef"))
mat.sex<- update(mat.sex,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(mat.sex)
#model could not converge

#remove some random effects
mat.sex1<-glmer(mature~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+(1|cohort)+(1|mum)+(1|dad_bio),
               family=binomial,data=subset(offspring.mat,EP=="yes"))
ss <- getME(mat.sex1,c("theta","fixef"))
mat.sex1<- update(mat.sex1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
#still failed to converge

#try with just age effects
mat.sex2<-glmer(mature~Mage*sex+Bage*sex+Sage*sex+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               family=binomial,data=subset(offspring.mat,EP=="yes"))
ss <- getME(mat.sex2,c("theta","fixef"))
mat.sex2<- update(mat.sex2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(mat.sex2)

table(offspring.mat$mature)
table(offspring.mat$sex)
table(offspring$independent)
#problem: only 13 do not make it to maturity... (2156 do)

######################################################################################
#lifespan of those that make it to maturity
offspring.long<- offspring %>%
  filter(independent==1) %>%
  filter(fate=="Died")%>%
  filter(sex!="U")%>%
  filter(lifespan>0)

hist(offspring.long$lifespan)

#check for overdispersion
dispersionstats <- offspring.long %>%
  summarise(
  means = mean(lifespan),
  variances = var(lifespan),
  ratio = variances/means)
#data is pretty overdispersed

#if the data is skewed, try NB1, NB2 and do an AIC
Lnb2<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
               family=nbinom2(),data=subset(offspring.long,EP=="yes"))
Lnb1<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=nbinom1(),data=subset(offspring.long,EP=="yes"))
Lgenpois<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              family=genpois(link="log"),data=subset(offspring.long,EP=="yes"))
Lcompois<-glmmTMB(lifespan~MageREL*sex+BageREL*sex+SageREL*sex+SageM*sex+MageM*sex+BageM*sex+
                    (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  family=compois(link="log"),data=subset(offspring.long,EP=="yes"))

AICtab(Lnb2,Lnb1,Lgenpois,Lcompois)
#best model


  
  
  
  