# visualize seagrass data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("patchwork")

# load data
sg<-read.csv("wdata/p cover and shoot density by quadrat long.csv")
theme_set(theme_bw()+theme(panel.grid=element_blank()))

# look at change in % cover of different species over time by treatment
# need to create a change dataset
sg2<-sg%>%
  group_by(sampling,mnths,blockID,plotID,bay,scar,graze,taxa)%>%
  summarize(m.pc=mean(pcover,na.rm=T),
            sd.pc=sd(pcover,na.rm=T),
            m.sht=mean(sdensity,na.rm=T),
            sd.sht=sd(sdensity,na.rm=T))

sg2.1<-filter(sg2,sampling=="s1")%>%
  ungroup()%>%
  select(blockID,plotID,taxa,m.pc1=m.pc,sd.pc1=sd.pc,m.sht1=m.sht,sd.sht1=sd.sht)

sg.delta<-sg2%>%
  filter(sampling!="s1")%>%
  left_join(sg2.1)%>%
  mutate(delta.pc=m.pc-m.pc1,delta.pcsd=sd.pc-sd.pc1, delta.sht=m.sht-m.sht1,delta.shtsd=sd.sht-sd.sht1,
         pchange.pc=(m.pc-m.pc1)/m.pc1,
         pchange.sd=(m.sht-m.sht1)/m.sht1)

write.csv(sg.delta,"wdata/change in seagrass pc and sd.csv",row.names=F)

ggplot(data=sg.delta%>%
         filter(taxa=="T"),aes(x=sampling,y=delta.pc,group=plotID,color=scar))+
  geom_line(aes(group=plotID))+
  geom_point()+
  facet_grid(bay~graze)+
  geom_hline(aes(yintercept = 0))

ggplot(data=sg.delta%>%
         filter(taxa=="T"),aes(x=sampling,y=delta.pc,group=plotID,color=scar))+
  geom_line(aes(group=plotID))+
  geom_point()+
  facet_grid(bay~graze)+
  geom_hline(aes(yintercept = 0))

ggplot(data=sg.delta%>%
         filter(taxa=="T"),aes(x=sampling,y=delta.pc,group=plotID,fill=bay))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  facet_grid(scar~graze)+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  ylab("Change in Thalassia % Cover")+
  xlab("Sampling")

ggsave("figures/changeinthalassiapercentcover.png")

ggplot(data=sg.delta%>%
         filter(taxa=="T"),aes(x=sampling,y=100*pchange.pc,group=plotID,fill=bay))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  facet_grid(scar~graze)+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  #scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  scale_fill_manual(values=c("#61CBF4","#104862"),labels=c("St. Andrew","St. Joe"),name="Bay")+
  ylab("% Change in Thalassia % Cover")+
  xlab("Sampling")

ggsave("figures/pchangeinthalassiapercentcover.png")

ggplot(data=sg.delta%>%
         filter(taxa=="T")%>%
         filter(graze=="Graze")%>%
         filter(scar=="No.scar"),aes(x=sampling,y=100*pchange.pc,group=plotID,fill=bay))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  # facet_grid(scar~graze)+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  #scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  scale_fill_manual(values=c("#61CBF4","#104862"),labels=c("St. Andrew","St. Joe"),name="Bay")+
  ylab("% Change in Thalassia % Cover")+
  xlab("Sampling")

ggsave("figures/pchangeinthalassiapercentcover-onlyUG.png")

ggplot(data=sg.delta%>%
         filter(taxa=="T"),aes(x=sampling,y=delta.sht,group=plotID,fill=bay))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  facet_grid(scar~graze)+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  ylab("Change in Thalassia shoot density")+
  xlab("Sampling")

ggsave("figures/changeinthalassiashootdensity.png")

ggplot(data=sg.delta%>%
         filter(taxa=="T"),aes(x=sampling,y=100*pchange.sd,group=plotID,fill=bay))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  facet_grid(scar~graze)+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  #scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  scale_fill_manual(values=c("#61CBF4","#104862"),labels=c("St. Andrew","St. Joe"),name="Bay")+
  ylab("% Change in Thalassia shoot density")+
  xlab("Sampling")

ggsave("figures/pchangeinthalassiashootdensity.png")

ggplot(data=sg.delta%>%
         filter(taxa=="T")%>%
         filter(graze=="Graze")%>%
         filter(scar=="No.scar"),aes(x=sampling,y=100*pchange.sd,group=plotID,fill=bay))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  # facet_grid(scar~graze)+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  #scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  scale_fill_manual(values=c("#61CBF4","#104862"),labels=c("St. Andrew","St. Joe"),name="Bay")+
  ylab("% Change in Thalassia shoot density")+
  xlab("Sampling")

ggsave("figures/pchangeinthalassiashootdensity-onlyUG.png")

# look at change in % cover compared to original at s2

ggplot(data=sg.delta%>%
         filter(taxa=="T")%>%
         filter(graze=="Graze")%>%
         filter(scar=="No.scar")%>%
         filter(sampling=="s2"),
       aes(x=m.sht1,y=delta.sht))+
  geom_hline(aes(yintercept = 0))+
  geom_point()+
  geom_smooth(method="lm",color="black")+
  ylab("Change in shoot density after grazing")+
  xlab("Shoot density prior to grazing")

ggsave("figures/changein shoot density at s2.jpg")


