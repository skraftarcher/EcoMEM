# explore change over course of study



source("scripts/install_packages_function.R")

lp("tidyverse")
lp("vegan")

# load data----

delt<-read.csv("wdata/change over course of study.csv")
# set plot parameters----
theme_set(theme_bw()+theme(axis.title = element_text(size=16),
                           axis.text = element_text(size=14),
                           panel.grid = element_blank(),
                           legend.text = element_text(size=14),
                           legend.title = element_text(size=16)))

bp<-ggplot(data=delt%>%filter(graze=="Graze"))+
  geom_hline(aes(yintercept=0))+
  scale_color_manual(values=c("#66c2a5","#fc8d62"),name="Grazing Treatment")+
  theme(legend.position = "none")

# look at recovedelt# look at recovery of seagrass variables over time
(pc<-bp+
   geom_point(aes(x=sampling,y=100*pd.pcover,color=graze),size=5,alpha=.6)+
    geom_line(aes(x=sampling,y=100*pd.pcover,group=plotID,color=graze))+
    ylab("% change in cover"))

(sd<-bp+
    geom_point(aes(x=sampling,y=100*pd.sdens,color=graze),size=5,alpha=.6)+
    geom_line(aes(x=sampling,y=100*pd.sdens,group=plotID,color=graze))+
    ylab("% change in density"))  

(can<-bp+
    geom_point(aes(x=sampling,y=100*pd.can,color=graze),size=5,alpha=.6)+
    geom_line(aes(x=sampling,y=100*pd.can,group=plotID,color=graze))+
    ylab("% change in canopy height"))  

(grow<-bp+
    geom_point(aes(x=sampling,y=100*pd.grow,color=graze),size=5,alpha=.6)+
    geom_line(aes(x=sampling,y=100*pd.grow,group=plotID,color=graze))+
    ylab("% change in growth"))  

lp("patchwork")
(pc+sd)/(can+grow) 

ggsave("figures/change over time.jpg",height=9,width=10)

# look at first time seagrass response variable has a pchange > 0
delt2<-delt%>%
  select(plotID,sampling,pd.pcover,pd.sdens,pd.can,pd.grow)%>%
  mutate(rcover=ifelse(pd.pcover>=0,1,0),
         rdens=ifelse(pd.sdens>=0,1,0),
         rcan=ifelse(pd.can>=0,1,0),
         rgrow=ifelse(pd.grow>=0,1,0),
         sscore=case_when(
           sampling=="s2"~1,
           sampling=="s3"~.75,
           sampling=="s4"~.50,
           sampling=="s5"~.25),
         rcovers=rcover*sscore,
         rdenss=rdens*sscore,
         rcans=rcan*sscore,
         rgrows=rgrow*sscore)%>%
  group_by(plotID)%>%
  summarize(rcover=max(rcovers,na.rm=T),
            rdens=max(rdenss,na.rm=T),
            rcan=max(rcans,na.rm=T),
            rgrow=max(rgrow,na.rm=T))%>%
  mutate(recovery.score=(rcover+rdens+rcan+rgrow)/4,
         recovery.score.nopc=(rdens+rcan+rcover)/3)%>%
  select(plotID,recovery.score,recovery.score.nopc)

rec.sum<-read.csv("odata/allplotinfo.csv")%>%
  left_join(delt2)%>%
  group_by(scar,graze)%>%
  summarize(mrecovery.score=mean(recovery.score.nopc),
            cil=mrecovery.score-(1.96*sd(recovery.score.nopc)/sqrt(11)),
            cih=mrecovery.score+(1.96*sd(recovery.score.nopc)/sqrt(11)))%>%
  mutate(treatment=case_when(
    scar=="Scar" & graze=="Graze"~"Scar and Grazing",
    scar=="Scar" & graze=="No.graze"~"Scar",
    scar=="No.scar" & graze=="Graze"~"Grazing",
    TRUE~"Control"))


rec<-read.csv("odata/allplotinfo.csv")%>%
  left_join(delt2)%>%
  mutate(treatment=case_when(
    scar=="Scar" & graze=="Graze"~"Scar and Grazing",
    scar=="Scar" & graze=="No.graze"~"Scar",
    scar=="No.scar" & graze=="Graze"~"Grazing",
    TRUE~"Control"))

rec.sum$treatment<-factor(rec.sum$treatment,levels=c("Control",
                                                     "Scar",
                                                     "Grazing",
                                                     "Scar and Grazing"))
rec.sum<-rec.sum%>%
  arrange(treatment)

rec.aov<-aov(recovery.score.nopc~treatment,data=rec)

summary(rec.aov)

TukeyHSD(rec.aov)

ggplot(data=rec.sum)+
  geom_errorbar(aes(x=treatment,ymin=cil,ymax=cih),width=.1)+
  geom_bar(aes(x=treatment,y=mrecovery.score,fill=graze),stat="identity")+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"),
                    labels=c("Grazed","Not Grazed"),
                    name="Grazing Treatment")+
  annotate(geom="text",x=c(1,2,3,4),y=rec.sum$cih+.05,label = c("a","a.b","a,b","b"),size=5)+
  ylab("Recovery score")+
  xlab("")+
  theme(legend.position="none")

ggsave("figures/recoveryscorebytreatment.jpg")

ggplot(data=rec,aes(group=blockID,y=recovery.score.nopc,x=graze))+
  geom_point(size=4)+
  geom_line()+
  facet_grid(bay~scar)

# make sulfide intrusion figure
delt3<-delt[,c(1,5:11,46,17:32)]%>%
  filter(sampling=="s5")%>%
  distinct()%>%
  filter(!is.na(pre_d34S_rhizomes))%>%
  pivot_longer(10:25,names_to="n1",values_to="resp")%>%
  separate(n1,into=c("sampling","measure","component"),sep="_")%>%
  pivot_wider(names_from=measure,values_from=resp)%>%
  right_join(rec)

delt3$component<-factor(delt3$component,levels=c("roots","rhizomes","leaves"))

d34slab<-expression(paste(delta^"34"," S"))
                        
# pre sulfide intrusion
ggplot(delt3%>%
         filter(sampling=="pre"))+
  geom_point(aes(x=component,y=d34S),size=4)+
  geom_line(aes(x=component,y=d34S,group=plotID))+
  ylab(d34slab)+
  xlab("")

ggsave("figures/sulfurisotopes v1.jpg")


ggplot(delt3%>%
         filter(sampling=="pre")%>%
         filter(graze=="Graze"))+
  geom_point(aes(x=component,y=d34S,color=100*pd.pcover),size=4)+
  geom_line(aes(x=component,y=d34S,group=plotID,color=100*pd.pcover))+
  ylab(d34slab)+
  xlab("")+
  scale_color_viridis_c(option="turbo",name="% Change \n% Cover")

ggsave("figures/sulfurisotopes v2.jpg")

# see if there is a relationship between recovery score and porewater sulfides
par(mfrow=c(2,2))
pore<-read.csv("wdata/porewater_sulfides_plots.csv")%>%
  select(blockID,scar,graze,sampling,Concentration.um)%>%
  mutate(sampling=ifelse(sampling=="s4","winter.sulfide","summer.sulfide"))%>%
  group_by(blockID,scar,graze,sampling)%>%
  summarize(conc=mean(Concentration.um))%>%
  pivot_wider(names_from=sampling,values_from=conc)%>%
  left_join(rec)%>%
  left_join(delt%>%filter(sampling=="s5"))

sulflab<-expression(paste(" Porewater Sulfide (",mu,"M)"))

lp("lmerTest")
lp("performance")
wsulf.lm<-lm(pd.sdens~winter.sulfide,data=pore%>%
                   filter(graze=="Graze"))

plot(wsulf.lm)

summary(wsulf.lm)
r2(wsulf.lm)
wr2<-expression(paste("R"^"2"," = 0.22"))

ssulf.lm<-lm(pd.sdens~summer.sulfide,data=pore%>%
                   filter(graze=="Graze"))

plot(ssulf.lm)

summary(ssulf.lm)
r2(ssulf.lm)


sr2<-expression(paste("R"^"2"," = 0.17"))

(wsulf.pc<-ggplot(data=pore%>%filter(graze=="Graze"),
               aes(x=winter.sulfide,y=100*pd.pcover))+
    geom_hline(aes(yintercept=0))+
  geom_point(size=4)+
  geom_smooth(method="lm",color="black")+
  # scale_color_manual(values=c("#66c2a5","#fc8d62"),
  #                   labels=c("Grazed","Not Grazed"),
  #                   name="Grazing Treatment")+
  # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
  #                    labels=c("Grazed","Not Grazed"),
  #                    name="Grazing Treatment")+
  xlab(sulflab)+
  ylab("% Change - % Cover")+
    ggtitle("Winter"))

(wsulf.sd<-ggplot(data=pore%>%filter(graze=="Graze"),
                  aes(x=winter.sulfide,y=100*pd.sdens))+
    geom_hline(aes(yintercept=0))+
    geom_point(size=4)+
    geom_smooth(method="lm",color="black")+
    # scale_color_manual(values=c("#66c2a5","#fc8d62"),
    #                   labels=c("Grazed","Not Grazed"),
    #                   name="Grazing Treatment")+
    # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
    #                    labels=c("Grazed","Not Grazed"),
    #                    name="Grazing Treatment")+
    xlab(sulflab)+
    ylab("% Change - Density"))

(wsulf.can<-ggplot(data=pore%>%filter(graze=="Graze"),
                  aes(x=winter.sulfide,y=100*pd.can))+
    geom_hline(aes(yintercept=0))+
    geom_point(size=4)+
    geom_smooth(method="lm",color="black")+
    # scale_color_manual(values=c("#66c2a5","#fc8d62"),
    #                   labels=c("Grazed","Not Grazed"),
    #                   name="Grazing Treatment")+
    # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
    #                    labels=c("Grazed","Not Grazed"),
    #                    name="Grazing Treatment")+
    xlab(sulflab)+
    ylab("% Change - Canopy Height"))

(wsulf.grow<-ggplot(data=pore%>%filter(graze=="Graze"),
                  aes(x=winter.sulfide,y=100*pd.grow))+
    geom_hline(aes(yintercept=0))+
    geom_point(size=4)+
    geom_smooth(method="lm",color="black")+
    # scale_color_manual(values=c("#66c2a5","#fc8d62"),
    #                   labels=c("Grazed","Not Grazed"),
    #                   name="Grazing Treatment")+
    # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
    #                    labels=c("Grazed","Not Grazed"),
    #                    name="Grazing Treatment")+
    xlab(sulflab)+
    ylab("% Change - Growth"))

(wsulf.pc+wsulf.sd)/(wsulf.can+wsulf.grow)

ggsave("figures/porewater sulfide and recovery winter.jpg",width=10,height=8)

(ssulf.pc<-ggplot(data=pore%>%filter(graze=="Graze"),
               aes(x=summer.sulfide,y=100*pd.pcover))+
    geom_hline(aes(yintercept=0))+
  geom_point(size=4)+
  geom_smooth(method="lm",color="black")+
  # scale_color_manual(values=c("#66c2a5","#fc8d62"),
  #                    labels=c("Grazed","Not Grazed"),
  #                    name="Grazing Treatment")+
  # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
  #                   labels=c("Grazed","Not Grazed"),
  #                   name="Grazing Treatment")+
  xlab(sulflab)+
  ylab("% Change - % Cover")+
  ggtitle("Summer"))

(ssulf.sd<-ggplot(data=pore%>%filter(graze=="Graze"),
                  aes(x=summer.sulfide,y=100*pd.sdens))+
    geom_hline(aes(yintercept=0))+
    geom_point(size=4)+
    geom_smooth(method="lm",color="black")+
    # scale_color_manual(values=c("#66c2a5","#fc8d62"),
    #                    labels=c("Grazed","Not Grazed"),
    #                    name="Grazing Treatment")+
    # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
    #                   labels=c("Grazed","Not Grazed"),
    #                   name="Grazing Treatment")+
    xlab(sulflab)+
    ylab("% Change - Density"))

(ssulf.can<-ggplot(data=pore%>%filter(graze=="Graze"),
                  aes(x=summer.sulfide,y=100*pd.can))+
    geom_hline(aes(yintercept=0))+
    geom_point(size=4)+
    geom_smooth(method="lm",color="black")+
    # scale_color_manual(values=c("#66c2a5","#fc8d62"),
    #                    labels=c("Grazed","Not Grazed"),
    #                    name="Grazing Treatment")+
    # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
    #                   labels=c("Grazed","Not Grazed"),
    #                   name="Grazing Treatment")+
    xlab(sulflab)+
    ylab("% Change - Canopy Height"))


(ssulf.grow<-ggplot(data=pore%>%filter(graze=="Graze"),
                  aes(x=summer.sulfide,y=100*pd.grow))+
    geom_hline(aes(yintercept=0))+
    geom_point(size=4)+
    geom_smooth(method="lm",color="black")+
    # scale_color_manual(values=c("#66c2a5","#fc8d62"),
    #                    labels=c("Grazed","Not Grazed"),
    #                    name="Grazing Treatment")+
    # scale_fill_manual(values=c("#66c2a5","#fc8d62"),
    #                   labels=c("Grazed","Not Grazed"),
    #                   name="Grazing Treatment")+
    xlab(sulflab)+
    ylab("% Change - Growth"))

(ssulf.pc+ssulf.sd)/(ssulf.can+ssulf.grow)

ggsave("figures/porewater sulfide and recovery summer.jpg",width=10,height=8)



# look at change in root or rhizome sulfide levels from beginning to end of experiment

delt.s<-delt%>%
  select(blockID,plotID,scar,graze,
         pd.pcover,pd.sdens,pd.can,pd.grow,
         pre_d34S_rhizomes,pre_d34S_roots,
         final_d34S_rhizomes,final_d34S_roots,
         pre_d15N_leaves,final_d15N_leaves,
         pre_d15N_rhizomes,final_d15N_rhizomes)%>%
  distinct()%>%
  # filter(scar!="Scar")%>%
  left_join(rec)%>%
  pivot_longer(pre_d34S_rhizomes:final_d34S_roots,names_to="var",values_to = "d34s")%>%
  separate(var,into = c("sampling","meas","component"),sep="_")%>%
  pivot_wider(names_from = component,values_from = d34s)

pchange.iso<-delt%>%
  select(blockID,plotID,scar,graze, 
         pd.pcover,pd.sdens,pd.can,pd.grow,
         pre_d34S_rhizomes,pre_d34S_roots,
         final_d34S_rhizomes,final_d34S_roots,
         pre_d15N_leaves,final_d15N_leaves,
         pre_d15N_rhizomes,final_d15N_rhizomes)%>%
  distinct()%>%
  # filter(scar!="Scar")%>%
  mutate(change.root=case_when(
    pre_d34S_roots<0 & final_d34S_roots<0~(abs(pre_d34S_roots)-abs(final_d34S_roots)),
    pre_d34S_roots<0 & final_d34S_roots>0~(abs(pre_d34S_roots)-final_d34S_roots),
    pre_d34S_roots >0 & final_d34S_roots>0~(pre_d34S_roots-final_d34S_roots),
    pre_d34S_roots >0 & final_d34S_roots<0~(-1*(pre_d34S_roots+abs(final_d34S_roots)))),
    change.rhizomes=case_when(
      pre_d34S_rhizomes<0 & final_d34S_rhizomes<0~(abs(pre_d34S_rhizomes)-abs(final_d34S_rhizomes)),
    pre_d34S_rhizomes<0 & final_d34S_rhizomes>0~(abs(pre_d34S_rhizomes)-final_d34S_rhizomes),
    pre_d34S_rhizomes >0 & final_d34S_rhizomes>0~(pre_d34S_rhizomes-final_d34S_rhizomes),
    pre_d34S_rhizomes >0 & final_d34S_rhizomes<0~(-1*(pre_d34S_rhizomes-abs(final_d34S_rhizomes)))))%>%
  select(plotID,change.root,change.rhizomes)

delt.s<-left_join(delt.s,pchange.iso)

delt.s$sampling<-factor(delt.s$sampling,levels = c("pre","final"))

sulfisolab.root<-expression(paste(delta,"34 S in roots"))
sulfisolab.rhizomes<-expression(paste(delta,"34 S in rhizomes"))

(droot<-ggplot(delt.s%>%filter(graze=="Graze"))+
  geom_point(aes(x=sampling,y=roots,color=100*pd.pcover),size=6,alpha=.6)+
  geom_line(aes(x=sampling,y=roots,color=100*pd.pcover,group=plotID))+
  scale_color_viridis_c(option="turbo",name="% Change \n%Cover")+
  ylab(sulfisolab.root)+
  scale_x_discrete(labels=c("Preliminary","Final"),name=""))

(drhizome<-ggplot(delt.s%>%filter(graze=="Graze"))+
  geom_point(aes(x=sampling,y=rhizomes,color=100*pd.pcover),size=6,alpha=.6)+
  geom_line(aes(x=sampling,y=rhizomes,color=100*pd.pcover,group=plotID))+
  scale_color_viridis_c(option="turbo",name="% Change \n%Cover")+
  ylab(sulfisolab.rhizomes)+
  scale_x_discrete(labels=c("Preliminary","Final"),name=""))

droot+drhizome+plot_layout(guides = "collect")

ggsave("figures/changeinsulfurisotopes.jpg")

ggplot(delt.s%>%filter(plotID %in% c("SJ1UG","SJ1UU")))+
  geom_point(aes(x=sampling,y=roots,color=100*pd.pcover,shape=graze),size=6,alpha=.6)+
  geom_line(aes(x=sampling,y=roots,color=100*pd.pcover,group=plotID))+
  scale_color_viridis_c(option="turbo",name="% Change \n%Cover")+
  ylab(sulfisolab.root)+
  scale_shape_manual(values=c(16,17),name="Grazing",labels=c("Grazing","No grazing"))+
  scale_x_discrete(labels=c("Preliminary","Final"),name="")

ggsave("figures/change_isotopes_sj1.jpg")

ggplot(delt.s,aes(x=change.root,y=recovery.score,color=graze))+
  geom_point()+
  geom_smooth(method="lm")

# look at recovery score and OM in the first sampling
delt4<-left_join(delt,rec)

ggplot(data=delt4%>%
         # filter(sampling!="s3")%>%
         filter(graze=="Graze"),
       aes(y=pn,x=sampling))+
  geom_point()+
  geom_line(aes(group=plotID))+
  facet_wrap(~blockID)

# multivariate characterization of the environment at the start and the end for the blocks
delt5<-delt%>%
  filter(treat %in% c("UU","UG"))

delt6<-delt5%>%
  group_by(plotID)%>%
  summarize(mpd.cov=mean(pd.pcover,na.rm=T),
            mpd.dens=mean(pd.sdens,na.rm=T),
            mpd.can=mean(pd.can,na.rm=T),
            mpd.grow=mean(pd.grow,na.rm=T))%>%
  left_join(delt5%>%filter(sampling=="s5")%>%select(plotID,pd.pcover,pd.sdens,pd.can,pd.grow))
  
pore2<-pore%>%
  ungroup()%>%
  select(plotID,summer.sulfide)

env<-delt5%>%
  filter(sampling=="s2")%>%
  select(plotID,
         s1OM,s1pn,s1pp,s1cn,s1.light_pre,graze)%>%
  mutate(graze=ifelse(graze=="Graze",1,0))%>%
  distinct()%>%
  left_join(pore2)%>%
  drop_na()

cor(env$s1.light_pre,env$s1pp)
delt6<-left_join(data.frame(plotID=env[,1]),delt6)

env.std<-decostand(env[,-1],method = "standardize")

env.rda<-rda(delt6[,6:9]~s1OM*graze+s1pn*graze+s1pp*graze+s1cn*graze+s1.light_pre*graze+summer.sulfide*graze,env.std)
mod00 <- rda(delt6[,6:9] ~ 1,env.std)

plot(env.rda)

RsquareAdj(env.rda)

env.forward <- ordiR2step(mod00, scope=formula(env.rda),
                            direction="forward", permutations=how(nperm=199))
RsquareAdj(env.forward)
env.backward <- ordistep(env.rda, direction="backward", permutations=how(nperm=199))
RsquareAdj(env.backward)
summary(env.backward)

pinfo<-read.csv("odata/allplotinfo.csv")

delt6b<-bind_cols(delt6,data.frame(scores(env.backward,choices = c(1,2),display = "sites")))%>%
  left_join(pinfo)
resp.scores<-data.frame(scores(env.backward,choices = c(1,2),display = "species"))%>%
  mutate(lab=row.names(.),
         lab2=case_when(
           lab=="pd.can"~"% Change - Canopy Height",
           lab=="pd.pcover"~"% Change - % Cover",
           lab=="pd.grow"~"% Change - Growth",
           lab=="pd.sdens"~"% Change - Density"
         ))
var.scores<-data.frame(scores(env.backward,choices = c(1,2),display = "bp"))%>%
  mutate(lab=row.names(.),
         lab2=case_when(
           lab=="s1OM"~"sediment OM",
           lab=="summer.sulfide"~"Sulfide uM",
           lab=="graze"~"Grazed",
           lab=="s1OM:graze"~"sediment OM * Grazed",
           lab=="graze:summer.sulfide"~"Sulfide uM * Grazed"
         ))

# var.scores$RDA2b<-var.scores$RDA2-c(0,0,.05,-.05)
# var.scores$RDA1b<-var.scores$RDA1-c(-.15,.15,0,0)

ggplot(data=delt6b)+
  # scale_color_viridis_d()+
  # geom_segment(aes(x=0,y=0,xend = RDA1,yend=RDA2),data=resp.scores[c(1,3),],color="red")+
  # geom_label(aes(x=RDA1,y=RDA2,label=lab2),data=resp.scores[c(1,3),],color="red")+
  geom_hline(aes(yintercept=0),linetype="dashed",color="grey")+
  geom_vline(aes(xintercept=0),linetype="dashed",color="grey")+
  geom_point(aes(x=RDA1,y=RDA2,color=100*pd.sdens,size=pd.grow))+
  geom_segment(aes(x=0,y=0,xend = RDA1,yend=RDA2),data=var.scores[-2,])+
  geom_label(aes(x=RDA1,y=RDA2,label=lab2),data=var.scores[-2,],alpha=.6)+
  coord_fixed(ratio=1)+
  ylim(-2.5,2.5)+
  xlim(-2,2.5)+
  scale_color_viridis_c(option = "turbo",name = "% Change \nDensity")+
  scale_size_continuous(name="% Change \nGrowth",breaks = c(-.7, 0, 1,1.8),
                        labels = c(-70,0,100,180))+
  ylab("RDA2")+
  xlab("RDA1")

ggsave("figures/multivariate pchange.jpg",height=6,width=7)

ggplot(data=delt6b)+
  # scale_color_viridis_d()+
  geom_segment(aes(x=0,y=0,xend = RDA1,yend=RDA2),data=resp.scores[c(1,3),],color="red")+
  geom_label(aes(x=RDA1,y=RDA2,label=lab2),data=resp.scores[c(1,3),],color="red")+
  geom_hline(aes(yintercept=0),linetype="dashed",color="grey")+
  geom_vline(aes(xintercept=0),linetype="dashed",color="grey")+
  geom_point(aes(x=RDA1,y=RDA2,color=pd.sdens,size=pd.grow))+
  # geom_segment(aes(x=0,y=0,xend = RDA1,yend=RDA2),data=var.scores)+
  # geom_label(aes(x=RDA1b,y=RDA2b,label=lab2),data=var.scores,alpha=.6)+
  coord_fixed(ratio=1)+
  ylim(-2.5,2.5)+
  xlim(-2,2.5)+
  scale_color_viridis_c(option = "turbo",name = "% Change \nDensity")+
  scale_size_continuous(name="% Change \nGrowth")+
  ylab("RDA2")+
  xlab("RDA1")

ggsave("figures/multivariate pchange _response only.jpg",height=8,width=8)


# % change seagrass metrics at end of study
s5<-delt%>%filter(sampling=="s5")%>%
  group_by(scar,graze,treat)%>%
  summarize(pd.pcoverse=sd(pd.pcover,na.rm=T)/sqrt(11),
            pd.sdensse=sd(pd.sdens,na.rm=T)/sqrt(11),
            pd.canse=sd(pd.can,na.rm=T)/sqrt(11),
            pd.growse=sd(pd.grow,na.rm=T)/sqrt(11),
            pd.pcover=mean(pd.pcover,na.rm=T),
            pd.sdens=mean(pd.sdens,na.rm=T),
            pd.can=mean(pd.can,na.rm=T),
            pd.grow=mean(pd.grow,na.rm=T))%>%
  mutate(treat=case_when(
    treat=="UU"~"Control",
    treat=="UG"~"Grazing",
    treat=="SU"~"Scar",
    treat=="SG"~"Scar & Grazing"))

s5$treat<-factor(s5$treat,levels=c("Control",
                                   "Scar",
                                   "Grazing",
                                   "Scar & Grazing"))

pcoverlm<-lmer(pd.pcover~scar*graze+(1|blockID),data=delt%>%filter(sampling=="s5"))
plot(pcoverlm)
summary(pcoverlm)

(pc5<-ggplot(data=s5,aes(x=treat,y=pd.pcover))+
    geom_hline(aes(yintercept = 0))+
    geom_errorbar(aes(ymin=pd.pcover-pd.pcoverse,
                      ymax=pd.pcover+pd.pcoverse),width=.1)+
    geom_bar(stat="identity",aes(fill=graze))+
    ylab("% Change - % Cover")+
    scale_fill_manual(values=c("#66c2a5","#fc8d62"),
                      labels=c("Grazed","Not Grazed"),
                      name="Grazing Treatment")+
    xlab(""))

sdenslm<-aov(pd.sdens~treat,data=delt%>%filter(sampling=="s5"))
plot(sdenslm)
summary(sdenslm)
anova(sdenslm)
TukeyHSD(sdenslm)

(sd5<-ggplot(data=s5,aes(x=treat,y=pd.sdens))+
    geom_hline(aes(yintercept = 0))+
    geom_errorbar(aes(ymin=pd.sdens-pd.sdensse,
                      ymax=pd.sdens+pd.sdensse),width=.1)+
    geom_bar(stat="identity",aes(fill=graze))+
    scale_fill_manual(values=c("#66c2a5","#fc8d62"),
                      labels=c("Grazed","Not Grazed"),
                      name="Grazing Treatment")+
    ylab("% Change - Density")+
    xlab(""))


(can5<-ggplot(data=s5,aes(x=treat,y=pd.can))+
    geom_hline(aes(yintercept = 0))+
    geom_errorbar(aes(ymin=pd.can-pd.canse,
                      ymax=pd.can+pd.canse),width=.1)+
    geom_bar(stat="identity",aes(fill=graze))+
    scale_fill_manual(values=c("#66c2a5","#fc8d62"),
                      labels=c("Grazed","Not Grazed"),
                      name="Grazing Treatment")+
    ylab("% Change - Canopy Height")+
    xlab(""))

(grow5<-ggplot(data=s5,aes(x=treat,y=pd.grow))+
    geom_hline(aes(yintercept = 0))+
    geom_errorbar(aes(ymin=pd.grow-pd.growse,
                      ymax=pd.grow+pd.growse),width=.1)+
    geom_bar(stat="identity",aes(fill=graze))+
    scale_fill_manual(values=c("#66c2a5","#fc8d62"),
                      labels=c("Grazed","Not Grazed"),
                      name="Grazing Treatment")+
    ylab("% Change - Growth")+
    xlab(""))

(pc5+sd5)/(can5+grow5)+plot_layout(guides="collect")

ggsave("figures/pchangeatend.jpg",width=12.5,height=7)

write.csv(delt%>%
            select(plotID,sampling,pd.pcover,pd.can,pd.sdens,pd.grow)%>%distinct(),"wdata/changedatatouse.csv",row.names = F)

