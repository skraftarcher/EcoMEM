# visualize core data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")
lp("lmerTest")

# load data----

d.m<-read.csv("wdata/change core morpho.csv")
d.bm<-read.csv("wdata/change_core_biomass.csv")%>%
  mutate(p.change=100*(delta.bm.g/pre.bm.g))
iso<-read.csv("wdata/first_batch_core_isotopes.csv")%>%
  mutate(component=case_when(
    component=="leaves"~"leaf.alive",
    component=="roots"~"roots.alive",
    component=="rhizomes"~"rhizome.alive",
    component=="sheaths"~"sheath.alive"))

iso1<-filter(iso,sampling=="pre")%>%
  select(-sID,-sampling,-date)%>%
  rename(sample.type=component)%>%
  distinct()
iso2<-filter(iso,sampling=="final")%>%
  select(-sID,-sampling,-date)%>%
  rename(sample.type=component)%>%
  distinct()

colnames(iso1)[c(1:4,7)]<-paste0("pre.",colnames(iso1)[c(1:4,7)])
colnames(iso2)[c(1:4,7)]<-paste0("post.",colnames(iso2)[c(1:4,7)])

d.bm2<-full_join(iso1,iso2)%>%
  full_join(d.bm)%>%
  mutate(pre.C=ifelse(pre.bm.g==0,0,pre.PC*pre.bm.g),
         post.C=ifelse(post.bm.g==0,0,post.PC*post.bm.g),
         delta.C=post.C-pre.C)%>%
  filter(sample.type %in% unique(iso2$sample.type))%>%
  mutate(ab=case_when(
    sample.type %in% c("leaf.alive","sheath.alive") ~"above",
    sample.type %in% c("rhizome.alive","roots.alive")~"below"))

theme_set(theme_bw()+theme(panel.grid=element_blank()))
# visualize change in aboveground biomass

(ab.d<-ggplot(data=d.bm%>%
                filter(sample.type=="leaf.alive")%>%
                filter(sg.sp=="T")%>%
                filter(pre.bm.g>0))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=delta.bm.g),width=.1,size=3)+
    geom_violin(aes(x=graze,y=delta.bm.g),alpha=.5)+
    facet_grid(scar~bay))


(e.d<-ggplot(data=d.bm%>%
                filter(sample.type=="epiphytes")%>%
                filter(sg.sp=="T")%>%
                filter(pre.bm.g>0))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=delta.bm.g),width=.1,size=3)+
    geom_violin(aes(x=graze,y=delta.bm.g),alpha=.5)+
    facet_grid(scar~bay))

(ab.pd<-ggplot(data=d.bm%>%
                filter(sample.type=="leaf.alive")%>%
                filter(sg.sp=="T")%>%
                 filter(pre.bm.g>0))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=p.change),width=.1,size=3)+
    geom_violin(aes(x=graze,y=p.change),alpha=.5)+
    facet_grid(scar~bay))

ab.aov<-aov(delta.bm.g~scar*graze*bay,data=d.bm%>%
              filter(sample.type=="leaf.alive")%>%
              filter(sg.sp=="T")%>%
              filter(pre.bm.g>0))
par(mfrow=c(2,2))
plot(ab.aov)
summary(ab.aov)

# look at leaf carbon
(lc<-ggplot(data=d.bm2%>%
             filter(sample.type=="leaf.alive"))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=delta.C),width=.1)+
  facet_grid(scar~bay))

(rootc<-ggplot(data=d.bm2%>%
              filter(sample.type=="roots.alive"))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=delta.C),width=.1)+
    facet_grid(scar~bay))

(rhic<-ggplot(data=d.bm2%>%
                 filter(sample.type=="rhizome.alive"))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=delta.C),width=.1)+
    facet_grid(scar~bay))

ab.c<-d.bm2%>%
  group_by(plotID,blockID,bay,scar,graze,ab)%>%
  summarize(tot.c=sum(delta.C,na.rm = T))

t.c<-d.bm2%>%
  group_by(plotID,blockID,bay,scar,graze)%>%
  summarize(tot.c=sum(delta.C,na.rm = T))

(tot.c<-ggplot(data=ab.c)+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=tot.c,color=ab),width=.1)+
    facet_grid(scar~bay))


(tot.c<-ggplot(data=t.c)+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=tot.c),width=.1)+
    facet_grid(scar~bay))

tc.aov<-aov(tot.c~scar*graze+bay,data=t.c)
plot(tc.aov)
summary(tc.aov)
TukeyHSD(tc.aov)

# below ground# below groundt.c

(b.d<-ggplot(data=d.bm%>%
                filter(sample.type=="below.ground")%>%
                filter(sg.sp=="T"))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=delta.bm.g),width=.1,size=3)+
    geom_violin(aes(x=graze,y=delta.bm.g),alpha=.5)+
    facet_grid(scar~bay))

(b.pd<-ggplot(data=d.bm%>%
                 filter(sample.type=="below.ground")%>%
                 filter(sg.sp=="T")%>%
                filter(pre.bm.g>0))+
    geom_hline(aes(yintercept=0))+
    geom_jitter(aes(x=graze,y=p.change),width=.1,size=3)+
    geom_violin(aes(x=graze,y=p.change),alpha=.5)+
    facet_grid(scar~bay))
