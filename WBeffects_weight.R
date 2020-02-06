offspring <- read.csv("C:/Users/u6354548/OneDrive - Australian National University/transgen-wren/study_born.csv")


#including julian incube
weightJ<-lmer(weight~MageREL+BageREL+SageREL+Jincube+MageM+BageM+SageM+weight.age+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=subset(offspring,EP=="yes"))
summary(weightJ)
#nothing significant

#excluding julian incube
weight<-lmer(weight~MageREL+BageREL+SageREL+MageM+BageM+SageM+weight.age+
                (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
              data=subset(offspring,EP=="yes"))
summary(weight)
#nothing significant