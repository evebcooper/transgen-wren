#Graphs for the AES/HDR conferences
#the effects of paternal age on offspring fitness
library(ggeffects)
library(ggplot2)
#Bage*EP interaction
summary(inde.all1)

pred<-ggpredict(inde.all1,terms=c("Bage[1:12]","EP"),type="fe")  #%>% plot
y<-c(predict(inde.all1,type="response"))
offspring.com<-offspring%>%
  filter(!is.na(Bage))%>%
  filter(!is.na(Mage))%>%
  filter(!is.na(Sage))%>%
  filter(!is.na(EP))%>%
  filter(!is.na(dad_bio))%>%
  filter(!is.na(mum))%>%
  filter(!is.na(dad_soc))%>%
  filter(!is.na(cohort))
Bage<-offspring.com$Bage
EP<-offspring.com$EP
pred.all<-tibble(y,Bage,EP)
pred<-pred %>%
  rename(Paternity=group)%>%
  mutate(Paternity=ifelse(Paternity=="no","Within-Pair","Extra-Pair"))

ggplot(pred,aes(x=x,y=predicted,colour=Paternity,fill=Paternity))+
  theme_classic()+
  scale_fill_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  scale_colour_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  geom_line(size=2)+
  geom_ribbon(data=pred,aes(ymin=conf.low,ymax=conf.high,colour=Paternity),alpha=0.2,linetype=0)+
   labs( x = "Age of Genetic Father", y = "Offspring Survival",title ="Genetic Father")+
  scale_x_continuous(limits=c(1,12),breaks=c(2,4,6,8,10,12))+
  scale_y_continuous(limits=c(0.2,0.8))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))
ggsave("Bage_EP_WP.jpeg", width = 6, height = 4)
#this graph suggests that parental care or parental environment effects improve with age,
#while older father do not have inherently better genes and may even decline slightly with age

#only WP Bage
ggplot(subset(pred,(Paternity=="Within-Pair")),aes(x=x,y=predicted,colour=Paternity,fill=Paternity,line=Paternity))+
  scale_colour_manual(values=c("blue"))+
  scale_fill_manual(values=c("blue"))+
  geom_line(size=2)+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.2,linetype=0)+
  labs( x = "Age of Genetic Father", y = "Offspring Survival",
        title ="Genetic Father")+
  theme_classic()+
  scale_x_continuous(limits=c(1,12),breaks=c(2,4,6,8,10,12))+
  scale_y_continuous(limits=c(0.2,0.8))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))
ggsave("Bage_WP.jpeg", width = 6, height = 4)

################################
#Sage*EP interaction
summary(inde.all2)
#significant interaction
pred.s<-ggpredict(inde.all2,terms=c("Sage[1:12]","EP"),type="fe")  #%>%plot()

pred.s<-pred.s %>%
  rename(Paternity=group)%>%
  mutate(Paternity=ifelse(Paternity=="no","Within-Pair","Extra-Pair"))

ggplot(pred.s,aes(x=x,y=predicted,colour=Paternity,fill=Paternity))+
  scale_fill_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  scale_colour_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  geom_line(size=2)+
  geom_ribbon(data=pred.s,aes(ymin=conf.low,ymax=conf.high,colour=Paternity),alpha=0.2,linetype=0)+
  labs( x = "Age of Social Father", y = "Offspring Survival",title ="Social Father")+
  theme_classic()+
  scale_x_continuous(limits=c(1,12),breaks=c(2,4,6,8,10,12))+
  scale_y_continuous(limits=c(0.2,0.8))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))
ggsave("Sage_EP_WP.jpeg", width = 6, height = 4)

#within pair Sage only
ggplot(subset(pred.s,(Paternity=="Within-Pair")),aes(x=x,y=predicted,colour=Paternity,fill=Paternity,line=Paternity))+
  scale_colour_manual(values=c("blue"))+
  scale_fill_manual(values=c("blue"))+
  geom_line(size=2)+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.2,linetype=0)+
  labs( x = "Age of Social Father", y = "Offspring Survival",
        title ="Social Father")+
  theme_classic()+
  scale_x_continuous(limits=c(1,12),breaks=c(2,4,6,8,10,12))+
  scale_y_continuous(limits=c(0.2,0.8))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))
ggsave("Sage_WP.jpeg", width = 6, height = 4)

##############################
#independence - EP only

#




###############################################
#weight at banding

#Bage*EP
weight.all<-lmer(weight~Mage+Bage*EP+Sage+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                 data=offspring)
summary(weight.all)

pred.wb<-ggpredict(weight.all,terms=c("Bage[1:12]","EP"),type="fe")  #%>%plot()
pred.wb<-pred.wb %>%
  rename(Paternity=group)%>%
  mutate(Paternity=ifelse(Paternity=="no","Within-Pair","Extra-Pair"))

ggplot(pred.wb,aes(x=x,y=predicted,colour=Paternity,fill=Paternity))+
  scale_fill_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  scale_colour_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  geom_line(size=2)+
  geom_ribbon(data=pred.wb,aes(ymin=conf.low,ymax=conf.high,colour=Paternity),alpha=0.2,linetype=0)+
  labs( x = "Age of Genetic Father", y = "Offspring Weight",title ="Genetic Father")+
  theme_classic()+
  scale_x_continuous(limits=c(1,12),breaks=c(2,4,6,8,10,12))+
  scale_y_continuous(limits=c(6.6,7.6))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))

ggsave("Bage_weight.jpeg", width = 6, height = 4)



#sage*EP
weight.allb<-lmer(weight~Mage+Bage+Sage*EP+weight.age+(1|cohort)+(1|mum)+(1|dad_bio)+(1|dad_soc),
                  data=offspring)
summary(weight.allb)

pred.ws<-ggpredict(weight.allb,terms=c("Sage[1:12]","EP"),type="fe")  #%>%plot()
pred.ws<-pred.ws %>%
  rename(Paternity=group)%>%
  mutate(Paternity=ifelse(Paternity=="no","Within-Pair","Extra-Pair"))

ggplot(pred.ws,aes(x=x,y=predicted,colour=Paternity,fill=Paternity))+
  scale_fill_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  scale_colour_manual(values=c("darkred","blue"),guide = guide_legend(reverse=TRUE))+
  geom_line(size=2)+
  geom_ribbon(data=pred.ws,aes(ymin=conf.low,ymax=conf.high,colour=Paternity),alpha=0.2,linetype=0)+
  labs( x = "Age of Social Father", y = "Offspring Weight",title ="Social Father")+
  theme_classic()+
  scale_x_continuous(limits=c(1,12),breaks=c(2,4,6,8,10,12))+
  scale_y_continuous(limits=c(6.6,7.6))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))

ggsave("Sage_weight.jpeg", width = 6, height = 4)

################################################
#EP Bage, EP Sage, and WP in the same model

#weight
summary(weight.sep)

pred.BEP<-ggemmeans(weight.sep1,terms=c("Bage"),type="fe", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.BWP<-ggemmeans(weight.sep1,terms=c("Bage"),type="fe",condition=c(WPb="1",EPb="0"))
pred.S<-ggemmeans(weight.sep1,terms=c("Sage"),type="fe",condition=c(EPb="1",WPb="0"))

ggplot(pred.BEP,aes(x=x,y=predicted))+
  geom_line(pred.S,mapping=aes(x=x,y=predicted), size=2,colour="blue",linetype="dashed")+
  geom_ribbon(data=pred.S,aes(ymin=conf.low,ymax=conf.high),alpha=0.3,fill="blue",colour="blue")+
  geom_line(size=2,colour="red",linetype="dashed")+
  geom_ribbon(data=pred.BEP,mapping=aes(ymin=conf.low,ymax=conf.high),alpha=0.4,fill="red",colour="red")+
  geom_line(pred.BWP,mapping=aes(x=x,y=predicted), size=2,colour="lightblue",linetype="dashed")+
  geom_ribbon(data=pred.BWP,aes(ymin=conf.low,ymax=conf.high),alpha=0.4,fill="lightblue",colour="lightblue")+
  labs( x = "Father Age", y = "Offspring Weight",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  #scale_y_continuous(limits=c(0.05,0.09))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))
#although EP Bage has a negative effect, and EP Sage has no effect -
#surprisingly, WP faher age has a positive effect on offspring weight

#independence - including all
summary(inde.sep)

pred.BEP<-ggemmeans(inde.sep1,terms=c("Bage"),type="fe", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.BWP<-ggemmeans(inde.sep1,terms=c("Bage"),type="fe",condition=c(WPb="1",EPb="0"))
pred.S<-ggemmeans(inde.sep1,terms=c("Sage"),type="fe",condition=c(EPb="1",WPb="0"))

ggplot(pred.BEP,aes(x=x,y=predicted))+
  geom_line(pred.S,mapping=aes(x=x,y=predicted), size=2,colour="blue",linetype="dashed")+
  geom_ribbon(data=pred.S,aes(ymin=conf.low,ymax=conf.high),alpha=0.3,fill="blue",colour="blue")+
  geom_line(size=2,colour="darkgreen",linetype="dashed")+
  geom_ribbon(data=pred.BEP,mapping=aes(ymin=conf.low,ymax=conf.high),alpha=0.4,fill="darkgreen",colour="darkgreen")+
  geom_line(pred.BWP,mapping=aes(x=x,y=predicted), size=2,colour="red",linetype="dashed")+
  geom_ribbon(data=pred.BWP,aes(ymin=conf.low,ymax=conf.high),alpha=0.4,fill="red",colour="red")+
  labs( x = "Father Age", y = "Offspring Survival",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.25,0.7))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))
#despite both EP Sage and Bage showing negative trends,
#WP father shows a positive effect

#independence - excluding WP
pred.BEP<-ggemmeans(inde.sep1,terms=c("Bage"),type="fe", condition=c(EPb="1",WPb="0"))  #%>%plot()
pred.S<-ggemmeans(inde.sep1,terms=c("Sage"),type="fe",condition=c(EPb="1",WPb="0"))

ggplot(pred.BEP,aes(x=x,y=predicted))+
  geom_line(pred.S,mapping=aes(x=x,y=predicted), size=2,colour="blue",linetype="dashed")+
  geom_ribbon(data=pred.S,aes(ymin=conf.low,ymax=conf.high),alpha=0.3,fill="blue",colour="blue")+
  geom_line(size=2,colour="darkgreen",linetype="dashed")+
  geom_ribbon(data=pred.BEP,mapping=aes(ymin=conf.low,ymax=conf.high),alpha=0.4,fill="darkgreen",colour="darkgreen")+
  labs( x = "Father Age", y = "Offspring Survival",title ="")+
  theme_classic()+
  scale_x_continuous(limits=c(1,10),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0.25,0.7))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5))

#despite both EP Sage and Bage showing negative trends,
#WP father shows a positive effect

