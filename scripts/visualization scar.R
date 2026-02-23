# visualize scar data
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("patchwork")

# load data
scar<-read.csv("wdata/pchange_scar_byblock.csv")
theme_set(theme_bw()+theme(panel.grid=element_blank(),
                           axis.title = element_text(size=18),
                           axis.text = element_text(size=16)))

ggplot(data=scar,aes(x=sampling,y=-100*l.pchange,group=blockID,fill=bay))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  #scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  scale_fill_manual(values=c("#61CBF4","#104862"),labels=c("St. Andrew","St. Joe"),name="Bay")+
  ylab("% recovery (scar length)")+
  xlab("Sampling")

ggsave("figures/pchangeinscalength_bar.png")


ggplot(data=scar,aes(x=sampling,y=-100*l.pchange,group=blockID,color=bay))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept=1.5),linetype="dashed")+
  geom_vline(aes(xintercept=2.5),linetype="dashed")+
  geom_vline(aes(xintercept=3.5),linetype="dashed")+
  #scale_fill_viridis_d(begin=.2,end=.8,name="Bay",labels=c("St. Andrew","St. Joe"))+
  scale_color_manual(values=c("#61CBF4","#104862"),labels=c("St. Andrew","St. Joe"),name="Bay")+
  ylab("% recovery in scar length")+
  xlab("Sampling")

# see if recovery of scar and seagrass changes also correlate
sg<-read.csv("wdata/change in seagrass pc and sd.csv")

sg2<-sg%>%
  filter(graze=="Graze")%>%
  filter(scar=="No.scar")%>%
  filter(taxa=="T")

scar2<-left_join(scar,sg2)%>%
  mutate(l.pchange2=-100*l.pchange)

summary(lm(delta.sht~l.pchange2,data=scar2))
r2<-expression(paste("R"^"2"," = 0.15"))

ggplot(data=scar2,
       aes(x=-100*l.pchange,y=delta.sht))+
  geom_hline(aes(yintercept = 0))+
  geom_point()+
  geom_smooth(method="lm",color="black")+
  ylab("Change in shoot density after grazing")+
  xlab("% recovery in scar length")+
  annotate(geom="text",x=-6,y=7,label=r2,size=8)

ggsave("figures/scarrecoverandshootdensity.jpg")
