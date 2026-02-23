# visualize sulfure isotopes

library(tidyverse)
library(readxl)

# load data
ciso<-read.csv("wdata/core_isotopes.csv")%>%
  separate(plotID,sep = -2,into=c("blockID","ex"),remove=F)%>%
  select(-ex)

ciso$sampling<-factor(ciso$sampling,levels=c("pre","final"))

# look at sulfur isotopes by plot block and scar/graze in rhizomes

ggplot(data=ciso%>%filter(component=="rhizomes"),
       aes(x=blockID,y=d34S,fill=sampling))+
  geom_bar(stat="identity",position=position_dodge())+
  facet_grid(graze~bay,scales = "free")

ggplot(data=ciso%>%filter(component=="rhizomes"),
       aes(group=blockID,y=PS,x=sampling))+
  geom_point()+
  geom_line()+
  facet_grid(~graze,scales = "free")


ggplot(data=ciso%>%filter(component=="rhizomes"),
       aes(group=blockID,y=d13C,x=sampling))+
  geom_point()+
  geom_line()+
  facet_grid(bay~graze,scales = "free")

# look at sulfer isotopes by part of plant
ciso$component<-factor(ciso$component,levels = c("roots","rhizomes","leaves","sheaths"))

ggplot(data=ciso%>%
         filter(component!="sheaths")%>%
         filter(scar=="No scar"),
       aes(x=component,y=d34S,color=graze))+
  geom_line(aes(group=paste(plotID,sampling)))+
  geom_hline(aes(yintercept=-20),linetype="dashed",color="brown")+
  geom_hline(aes(yintercept=21),linetype="dashed",color="blue")+
  geom_point(aes(size=PS))+
  #geom_smooth(aes(x=as.numeric(component),y=d34S,color=sampling),method="lm")+
  facet_grid(bay~sampling,scales="free")+
  scale_color_manual(values=c("#66c2a5","#fc8d62"),name="Grazing Treatment")

