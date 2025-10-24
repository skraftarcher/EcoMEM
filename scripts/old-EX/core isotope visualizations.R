# visualize core isotope data
# load packages
library(tidyverse)

# load data
dat<-read.csv("wdata/first_batch_core_isotopes.csv")

dat$sampling<-factor(dat$sampling,levels = c("pre","final"))

theme_set(theme_bw()+theme(panel.grid = element_blank()))

# look at CN by plot over time----
#leaves
(cn.l<-ggplot(data=dat%>%
         filter(component=="leaves"))+
  geom_point(aes(x=sampling,y=CN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=CN,group=plotID,color=bay))+
  facet_grid(scar~graze))

#rhizomes
(cn.rh<-ggplot(data=dat%>%
         filter(component=="rhizomes"))+
  geom_point(aes(x=sampling,y=CN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=CN,group=plotID,color=bay))+
  facet_grid(scar~graze))

# roots

(cn.r<-ggplot(data=dat%>%
         filter(component=="roots"))+
  geom_point(aes(x=sampling,y=CN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=CN,group=plotID,color=bay))+
  facet_grid(scar~graze))

# sheaths

(cn.s<-ggplot(data=dat%>%
         filter(component=="sheaths"))+
  geom_point(aes(x=sampling,y=CN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=CN,group=plotID,color=bay))+
  facet_grid(scar~graze))

#look at %C over time----
(pc.l<-ggplot(data=dat%>%
         filter(component=="leaves"))+
  geom_point(aes(x=sampling,y=PC,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PC,group=plotID,color=bay))+
  facet_grid(scar~graze))

(pc.rh<-ggplot(data=dat%>%
         filter(component=="rhizomes"))+
  geom_point(aes(x=sampling,y=PC,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PC,group=plotID,color=bay))+
  facet_grid(scar~graze))

(pc.r<-ggplot(data=dat%>%
         filter(component=="roots"))+
  geom_point(aes(x=sampling,y=PC,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PC,group=plotID,color=bay))+
  facet_grid(scar~graze))

(pc.s<-ggplot(data=dat%>%
         filter(component=="sheaths"))+
  geom_point(aes(x=sampling,y=PC,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PC,group=plotID,color=bay))+
  facet_grid(scar~graze))

# look at c13----
(c13.l<-ggplot(data=dat%>%
         filter(component=="leaves"))+
  geom_point(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  facet_grid(scar~graze))

(c13.rh<-ggplot(data=dat%>%
         filter(component=="rhizomes"))+
  geom_point(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  facet_grid(scar~graze))

(c13.r<-ggplot(data=dat%>%
         filter(component=="roots"))+
  geom_point(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  facet_grid(scar~graze))

(c13.s<-ggplot(data=dat%>%
         filter(component=="sheaths"))+
  geom_point(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=d13C,group=plotID,color=bay))+
  facet_grid(scar~graze))

# look at %N
(pn.l<-ggplot(data=dat%>%
         filter(component=="leaves"))+
  geom_point(aes(x=sampling,y=PN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PN,group=plotID,color=bay))+
  facet_grid(scar~graze))

(pn.rh<-ggplot(data=dat%>%
         filter(component=="rhizomes"))+
  geom_point(aes(x=sampling,y=PN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PN,group=plotID,color=bay))+
  facet_grid(scar~graze))

(pn.r<-ggplot(data=dat%>%
         filter(component=="roots"))+
  geom_point(aes(x=sampling,y=PN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PN,group=plotID,color=bay))+
  facet_grid(scar~graze))


(pn.s<-ggplot(data=dat%>%
         filter(component=="sheaths"))+
  geom_point(aes(x=sampling,y=PN,group=plotID,color=bay))+
  geom_line(aes(x=sampling,y=PN,group=plotID,color=bay))+
  facet_grid(scar~graze))

# n15 now----
(n15.l<-ggplot(data=dat%>%
                filter(component=="leaves"))+
   geom_point(aes(x=sampling,y=d15N,group=plotID,color=bay))+
   geom_line(aes(x=sampling,y=d15N,group=plotID,color=bay))+
   facet_grid(scar~graze))

(n15.rh<-ggplot(data=dat%>%
                 filter(component=="rhizomes"))+
    geom_point(aes(x=sampling,y=d15N,group=plotID,color=bay))+
    geom_line(aes(x=sampling,y=d15N,group=plotID,color=bay))+
    facet_grid(scar~graze))

(n15.r<-ggplot(data=dat%>%
                 filter(component=="roots"))+
    geom_point(aes(x=sampling,y=d15N,group=plotID,color=bay))+
    geom_line(aes(x=sampling,y=d15N,group=plotID,color=bay))+
    facet_grid(scar~graze))

(n15.s<-ggplot(data=dat%>%
                 filter(component=="sheaths"))+
    geom_point(aes(x=sampling,y=d15N,group=plotID,color=bay))+
    geom_line(aes(x=sampling,y=d15N,group=plotID,color=bay))+
    facet_grid(scar~graze))

# all leave plots----
library(patchwork)
cn.l+pc.l+pn.l+c13.l+n15.l+plot_layout(guides="collect")

# all rhizomes plots----
cn.rh+pc.rh+pn.rh+c13.rh+n15.rh+plot_layout(guides="collect")

# all root plots----
cn.r+pc.r+pn.r+c13.r+n15.r+plot_layout(guides="collect")

#all sheath plots
cn.s+pc.s+pn.s+c13.s+n15.s+plot_layout(guides="collect")


# make biplot
bidat<-dat%>%
  group_by(sampling,component,bay,scar,graze)%>%
  summarize(md15=mean(d15N),
            sdd15=sd(d15N),
            md13=mean(d13C),
            sdd13=sd(d13C))

ggplot(data=bidat%>%filter(component=="leaves"))+
  geom_errorbar(aes(x=md13,ymin=md15-sdd15,ymax=md15+sdd15,color=graze))+
  geom_errorbarh(aes(y=md15,xmin = md13-sdd13,xmax=md13+sdd13,color=graze))+
  geom_point(aes(x=md13,y=md15,color=graze,shape=scar),size=3)+
  facet_grid(sampling~bay,scales="free")

ggplot(data=bidat%>%filter(component=="rhizomes"))+
  geom_errorbar(aes(x=md13,ymin=md15-sdd15,ymax=md15+sdd15,color=graze))+
  geom_errorbarh(aes(y=md15,xmin = md13-sdd13,xmax=md13+sdd13,color=graze))+
  geom_point(aes(x=md13,y=md15,color=graze,shape=scar),size=3)+
  facet_grid(sampling~bay,scales="free")

ggplot(data=bidat%>%filter(component=="roots"))+
  geom_errorbar(aes(x=md13,ymin=md15-sdd15,ymax=md15+sdd15,color=graze))+
  geom_errorbarh(aes(y=md15,xmin = md13-sdd13,xmax=md13+sdd13,color=graze))+
  geom_point(aes(x=md13,y=md15,color=graze,shape=scar),size=3)+
  facet_grid(sampling~bay,scales="free")  

ggplot(data=bidat%>%filter(component=="sheaths"))+
  geom_errorbar(aes(x=md13,ymin=md15-sdd15,ymax=md15+sdd15,color=graze))+
  geom_errorbarh(aes(y=md15,xmin = md13-sdd13,xmax=md13+sdd13,color=graze))+
  geom_point(aes(x=md13,y=md15,color=graze,shape=scar),size=3)+
  facet_grid(sampling~bay,scales="free")


# look at isotopes by plot to see if it is worth looking at change canopy height
# are important for isotopes/nutrient concentration

dat<-dat%>%
  separate(plotID,into=c("bay","blockID","treat"),sep=c(2,3),remove=F)

ggplot(data=dat%>%
         filter(component=="leaves"))+
  geom_point(aes(x=d13C,y=d15N,color=graze,shape=sampling),size=3,alpha=.5)+
  geom_line(aes(x=d13C,y=d15N,color=graze,group=plotID))+
  facet_grid(blockID~bay,scales="free")


bidat2<-dat%>%
  group_by(sampling,component,bay,graze,blockID)%>%
  summarize(md15=mean(d15N),
            sdd15=sd(d15N),
            md13=mean(d13C),
            sdd13=sd(d13C))

ggplot(data=bidat2%>%filter(component=="leaves"))+
  geom_errorbar(aes(x=md13,ymin=md15-sdd15,ymax=md15+sdd15,color=graze))+
  geom_errorbarh(aes(y=md15,xmin = md13-sdd13,xmax=md13+sdd13,color=graze))+
  geom_point(aes(x=md13,y=md15,color=graze),size=3)+
  facet_grid(sampling~bay,scales="free")
