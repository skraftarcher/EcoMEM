# visualize visual survey data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("patchwork")
lp("vegan")
lp("lmerTest")
lp("ggeffects")

theme_set(theme_bw()+theme(panel.grid=element_blank(),
                           axis.text=element_text(size=14),
                           axis.title=element_text(size=18)))

# load data----
dat<-read.csv("wdata/wide_vissurvey_datawithdummy.csv")

dat2<-read.csv("wdata/changedatatouse.csv")%>%
  select(plotID,sampling,pd.pcover,pd.can,pd.sdens,pd.grow)

dat3<-read.csv("wdata/thalassia p cover shoot density and canopy by quadrat long.csv")%>%
  group_by(sampling,plotID)%>%
  summarize(mpcover=mean(pcover,na.rm=T),
            msdens=mean(sdensity,na.rm=T),
            mcan=mean(canopy,na.rm=T))

pinfo<-read.csv("odata/allplotinfo.csv")

com<-dat[,-1:-4]
env<-dat[,1:4]%>%
  mutate(treat2=case_when(
           treat=="UU"~"Control",
           treat=="UG"~"Grazing",
           treat=="SU"~"Scar",
           treat=="SG"~"Scar and Grazing"))%>%
  select(-treat)%>%
  left_join(dat3)%>%
  left_join(pinfo)

env$spr<-specnumber(com)-1
env$div<-diversity(com[,colnames(com)!="dummy"])

env$sampling<-factor(env$sampling)
env$treat2<-factor(env$treat2)
# univariate response

spr.lm<-lmer(spr~sampling*graze*bay+(1|blockID),
             data=env%>%mutate(sampling=relevel(sampling,ref="s1"))%>%
               filter(sampling %in% c("s1","s2","s5"))%>%
               filter(scar!="Scar"))
plot(spr.lm)
anova(spr.lm)
summary(spr.lm)

plot(ggeffect(spr.lm,terms=c("mpcover","sampling","bay")))

div.lm<-lmer(div~msdens*sampling+(1|blockID),data=env)
plot(div.lm)
anova(div.lm)
summary(div.lm)

# decrease in richness and diversity in sampling 2


com.hel<-decostand(com,"hellinger")

com.hel<-decostand(com[env$sampling %in% c("s1","s2","s5"),],"hellinger")

env<-env%>%
  mutate(mnths=case_when(
    sampling=="s1"~-1,
    sampling=="s2"~1,
    sampling=="s3"~6,
    sampling=="s4"~10,
    sampling=="s5"~13),
    prepost=ifelse(sampling=="s1",1,0))

com.rda<-rda(com.hel)

plot(com.rda)
summary(com.rda)

com.scores<-scores(com.mds,display="sites",choices=c(1,2))

env<-bind_cols(env,com.scores)

env1<-filter(env,sampling=="s1")%>%
  select(blockID,plotID,treat,graze,NMDS1.1=NMDS1,NMDS2.1 = NMDS2)

env2<-filter(env,sampling=="s2")%>%
  select(blockID,plotID,treat,graze,NMDS1.2=NMDS1,NMDS2.2 = NMDS2)

env3<-filter(env,sampling=="s3")%>%
  select(blockID,plotID,treat,graze,NMDS1.3=NMDS1,NMDS2.3 = NMDS2)

env4<-filter(env,sampling=="s4")%>%
  select(blockID,plotID,treat,graze,NMDS1.4=NMDS1,NMDS2.4 = NMDS2)

env5<-filter(env,sampling=="s5")%>%
  select(blockID,plotID,treat,graze,NMDS1.5=NMDS1,NMDS2.5 = NMDS2)


env.delt<-left_join(env1,env2)%>%
  left_join(env3)%>%
  left_join(env4)%>%
  left_join(env5)

ggplot(data=env.delt,aes(color=graze))+
  geom_point(aes(x=NMDS1.1,y=NMDS2.1),shape=16)+
  geom_point(aes(x=NMDS1.2,y=NMDS2.2),shape=17)+
  geom_segment(aes(x=NMDS1.1,y=NMDS2.1,xend=NMDS1.2,yend=NMDS2.2))+
  # geom_segment(aes(x=NMDS1.3,y=NMDS2.3,xend=NMDS1.2,yend=NMDS2.2))+
  # geom_segment(aes(x=NMDS1.3,y=NMDS2.3,xend=NMDS1.4,yend=NMDS2.4))+
  # geom_segment(aes(x=NMDS1.4,y=NMDS2.4,xend=NMDS1.5,yend=NMDS2.5))+
  facet_wrap(~blockID,scales="free")

# look at taxa responses
com2<-dat%>%
  pivot_longer(-1:-4,names_to = "taxa",values_to="abund")%>%
  filter(taxa!="dummy")%>%
  left_join(env)

graze.com<-com2%>%
  filter(scar!="Scar")%>%
  filter(sampling!="s1")%>%
  filter(blockID=="SJ1")%>%
  ungroup()%>%
  group_by(taxa)%>%
  summarise(abundance=sum(abund))%>%
  filter(abundance>1)

ggplot(data=com2%>%filter(scar!="Scar")%>%filter(sampling!="s1")%>%
         filter(blockID=="SJ1")%>%
         filter(taxa %in% graze.com$taxa),
       aes(x=abund))+
  geom_density(alpha=.4,aes(fill=graze))+
  facet_grid(graze~taxa,scales="free_x")

# filter down to only include taxa that were seen in more than 1 plot
com3<-com2%>%
  ungroup()%>%
  dplyr::select(plotID,taxa,abund)%>%
  distinct()%>%
  mutate(pres=ifelse(abund==0,0,1))%>%
  group_by(taxa)%>%
  summarize(nplots=sum(pres))%>%
  filter(nplots>5)

com4<-com2%>%
  ungroup()%>%
  dplyr::select(blockID,taxa,abund)%>%
  distinct()%>%
  mutate(pres=ifelse(abund==0,0,1))%>%
  group_by(taxa)%>%
  summarize(nplots=sum(pres))%>%
  filter(nplots>3)

taxa.use<-com4$taxa[com4$taxa %in% com3$taxa]

ggplot(data=com2%>%filter(taxa %in% taxa.use),aes(x=mpcover,y=abund))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use),aes(x=msdens,y=abund))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use),aes(x=mcan,y=abund))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~taxa,scales="free")

# try a cca use % cover, shoot density, canopy height, and water temp as environmental variables
# also only use taxa that were seen at more than 5 plots

wat<-read.csv("wdata/loggerdata summarized by block and sampling.csv")%>%
  mutate(bay=ifelse(bay=="St. Joe","SJ","SA"))

# use pre-grazed temps for s1 and post for all others
s1w<-filter(wat,sampling=="s1")%>%
  filter(prepost=="pre")%>%
  group_by(sampling,bay,blockID)%>%
  summarize(temp=mean(m.temp))

sw<-filter(wat,sampling!="s1")%>%
  group_by(sampling,bay,blockID)%>%
  summarize(temp=mean(m.temp))%>%
  bind_rows(s1w)

# get bay estimates of temp to fill these in (its a stand-in for season)
swb<-sw%>%
  ungroup()%>%
  group_by(sampling,bay)%>%
  summarize(btemp=mean(temp))


# there are a few blocks with NA for temp

com.cca<-left_join(com2,sw)%>%
  left_join(swb)%>%
  mutate(temp=ifelse(is.na(temp),btemp,temp))%>%
  dplyr::select(-btemp)%>%
  filter(taxa %in% taxa.use)%>%
  pivot_wider(names_from = taxa,values_from=abund,values_fill = 0)

env.cca<-com.cca[,1:14]
com.cca2<-com.cca[,-1:-14]

# first look at when and where there are 0 taxa
ggplot(env.cca,aes(x=spr,fill=sampling))+
  geom_histogram(position=position_dodge(0.9))

# lots of nothing out of RDA and CCA 
# look to see what individual taxa are doing more
ggplot(data=com2%>%filter(taxa %in% taxa.use[1:5]),aes(x=mcan,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[6:10]),aes(x=mcan,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[11:15]),aes(x=mcan,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[21:27]),aes(x=mcan,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[1:5]),aes(x=mpcover,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[6:10]),aes(x=mpcover,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[11:15]),aes(x=mpcover,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[21:27]),aes(x=mpcover,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[1:5]),aes(x=msdens,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[6:10]),aes(x=msdens,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[11:15]),aes(x=msdens,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use[21:27]),aes(x=msdens,y=abund,color=scar))+
  geom_point()+
  geom_smooth()+
  facet_grid(graze~taxa,scales="free")

ggplot(data=com2%>%filter(taxa %in% taxa.use),aes(x=sampling,y=abund,fill=treat2))+
  geom_boxplot()+
  facet_wrap(~taxa,scales="free")

# look at scallop abundance over time and treatment
# get sampling 1 out and change abundances to "preabund"

s1com<-com2%>%
  filter(sampling=="s1")%>%
  dplyr::select(plotID,taxa,preabund=abund,
                premsdens=msdens,
                prespr=spr)

comd<-com2%>%
  filter(sampling!="s1")%>%
  left_join(s1com)%>%
  mutate(pchange=100*((abund-preabund)/(preabund)),
         pchange=ifelse(preabund==0,abund*100,pchange),
         pchangesd=100*((msdens-premsdens)/(premsdens)),
         pchangespr=100*((spr-prespr)/(prespr)),
         pchangespr=ifelse(prespr==0,spr*100,pchangespr))


# look at change in species richness
spr<-lm(pchangespr~scar*pchangesd*sampling,data=comd%>%
          filter(taxa=="Argopecten.irradians")%>%
          filter(sampling %in% c("s2","s5")))

plot(spr)
summary(spr)
anova(spr)

div<-lm(div~scar+pchangesd+sampling,data=comd%>%
          filter(taxa=="Argopecten.irradians")%>%
          filter(sampling %in% c("s2","s5")))

plot(div)
summary(div)
anova(div)
lp("ggeffects")
plot(ggeffect(div,terms=c("pchangesd")))

scallops<-aov(pchange~graze*sampling,data=comd%>%
             filter(taxa=="Argopecten.irradians")%>%
               filter(sampling %in% c("s2","s5")))
par(mfrow=c(2,2))
plot(scallops)
summary(scallops)

TukeyHSD(scallops,which = "graze")

# look at msdens and scar and sampling
scallops2<-lm(pchange~scar*pchangesd*sampling,data=comd%>%
                filter(taxa=="Argopecten.irradians")%>%
                filter(sampling %in% c("s2","s5")))

plot(scallops2)
anova(scallops2)
summary(scallops2)

lp("ggeffects")
plot(ggeffect(scallops2,terms=c("pchangesd","scar","sampling")))
