#including weight residuals in survival model

#model the effects of Bage  etc. on weight to get residuals
weight.res<-lm(weight~weight.age+pre1992,
                data=offspring)
summary(weight.res)

residW<-resid(weight.res)

offspringFULL<-offspring %>%
  filter(!is.na(weight))%>%
  filter(!is.na(weight.age))

offspringFULL<-mutate(offspringFULL, weight.res=residW)

offspring<-left_join(offspring,select(offspringFULL,ID,weight.res),by="ID")


#add weight residuals to independence model
surv<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
                 EPb+Jincube+weight.res+
                 (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
               data=offspring, family="binomial")
summary(surv)
#n=4367
#weight.res is significant 
#WP Bage still significant

#check n against model controlling for weight and covariates
surv1<-glmer(independent~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+lifespanM+
              EPb+Jincube+weight+weight.age+pre1992+
              (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),control=glmerControl(optimizer ="bobyqa"),
            data=offspring, family="binomial")
summary(surv1)
#n =4363









  