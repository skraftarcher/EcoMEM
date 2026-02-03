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

com<-dat[,-1:-4]
env<-dat[,1:4]%>%
  mutate(scar=ifelse(treat %in% c("SG","SU"),"scar","no scar"),
         graze=ifelse(treat %in% c("UG","SG"),"graze","no graze"),
         treat2=case_when(
           treat=="UU"~"Control",
           treat=="UG"~"Grazing",
           treat=="SU"~"Scar",
           treat=="SG"~"Scar and Grazing"))%>%
  left_join(dat3)
env$treat<-factor(env$treat,levels = c("UU","SU","UG","SG"))

env$spr<-specnumber(com)-1
env$div<-diversity(com[,colnames(com)!="dummy"])

env$sampling<-factor(env$sampling)
# univariate response

spr.lm<-lmer(spr~sampling*msdens+(1|blockID),data=env%>%mutate(sampling=relevel(sampling,ref="s1")))
plot(spr.lm)
anova(spr.lm)
summary(spr.lm)

plot(ggeffect(spr.lm,terms=c("msdens","sampling")))

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
