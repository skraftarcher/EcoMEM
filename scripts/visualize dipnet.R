# explore dipnet data 

library(tidyverse)
library(vegan)

# load data
dip<-read.csv("wdata/feb32026_dipnet.csv")
theme_set(theme_bw()+theme(panel.grid = element_blank()))
change<-read.csv("wdata/change over course of study.csv")%>%
  dplyr::select(plotID,sampling,pd.sdens)%>%
  distinct()

# look at second sampling only for now
dip2<-filter(dip,sampling=="s2")%>%
  group_by(plotID,sampling,UpdateTaxaID,blockID,bay,scar,graze)%>%
  summarize(abundance=sum(abundance,na.rm=T))

dip2.wide<-dip2%>%
  left_join(change)%>%
  pivot_wider(names_from=UpdateTaxaID,values_from=abundance,values_fill = 0)

# filter to only include taxa seen in grazed plots
graze.dip<-dip2%>%
  filter(scar!="Scar")%>%
  ungroup()%>%
  group_by(UpdateTaxaID)%>%
  summarise(abundance=sum(abundance))%>%
  filter(abundance>1)

ggplot(data=dip2%>%filter(scar!="Scar")%>%
         filter(UpdateTaxaID %in% graze.dip$UpdateTaxaID),
       aes(x=abundance,y=graze,fill=graze))+
  geom_boxplot()+
  facet_wrap(~UpdateTaxaID,scales="free_x")

# look at how many times these taxa have been seen
graze.dip2<-dip2%>%
  filter(scar!="Scar")%>%
  filter(UpdateTaxaID %in% graze.dip$UpdateTaxaID)%>%
  ungroup()%>%
  group_by(graze,UpdateTaxaID)%>%
  summarise(nplot=n(),
            pplot=(nplot/12)*100)

ggplot(data=graze.dip2,
       aes(x=pplot,y=graze,color=graze))+
  geom_point()+
  facet_wrap(~UpdateTaxaID,scales="free_x")

# look at taxa richness
dip.env<-dip2.wide[,1:7]
dip.env$div<-diversity(dip2.wide[,-1:-7])
dip.env$spr<-specnumber(dip2.wide[,-1:-7])

ggplot(data=dip.env%>%filter(scar!="Scar"),aes(x=graze,y=spr,fill=graze))+
  geom_boxplot()+
  facet_wrap(~bay)

ggplot(data=dip.env%>%filter(scar!="Scar"),aes(x=graze,y=div,fill=graze))+
  geom_boxplot()+
  facet_wrap(~bay)

spr<-lm(spr~scar*pd.sdens,data=dip.env)
plot(spr)
summary(spr)

div<-lm(div~scar*pd.sdens,data=dip.env)
plot(div)
summary(div)
