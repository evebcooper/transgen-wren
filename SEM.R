library(piecewiseSEM)

offspring<-offspring %>%
  mutate(help.catN=ifelse(help.cat=="none",0,
                          ifelse(help.cat=="one",1,2)))

model <- psem(lm(y1 ~ x1, dat), lm(y1 ~ y2, dat), lm(y2 ~ x1, dat), lm(y3 ~ y1, dat))


model <- psem(lm(weight~ Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                   lifespanM+EPb+Jincube+weight.age+No.hatched+help.catN, offspring),
              lm(help.catN ~ Mage+Sage:EPb+Bage:WPb, offspring))
summary(model)


weight.sd<-lmer(weight~Mage+Bage:EPb+Sage:EPb+Bage:WPb+lifespanB:EPb+lifespanS:EPb+lifespanB:WPb+
                  lifespanM+EPb+Jincube+weight.age+pre1992+No.hatched+help.cat+
                  (1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                data=offspring,control=lmerControl(optimizer ="bobyqa"))
summary(weight.sd)