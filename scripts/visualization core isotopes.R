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
