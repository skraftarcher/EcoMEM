# Visualize initial change

source("scripts/install_packages_function.R")

lp("tidyverse")
lp("vegan")

# load data----
init<-read.csv("wdata/change from s1 to s2.csv")
init$scar<-factor(init$scar,levels=c("No.scar","Scar"))
init$graze<-factor(init$graze,levels=c("No.graze","Graze"))

# set plot parameters----
theme_set(theme_bw()+theme(axis.title = element_text(size=16),
                           axis.text = element_text(size=14),
                           panel.grid = element_blank(),
                           legend.text = element_text(size=14),
                           legend.title = element_text(size=16),
                           legend.position="top"))

bp<-ggplot(data=init)+
  scale_color_manual(values=c("#fc8d62","#66c2a5"),labels=c("No Grazing","Grazing"),
                     name="Grazing Treatment")
# look at correlations between potential descriptors - light, sulfer, OM, CN, d15N roots, d15N leaves,
#
pairs(init[,c(7,8,28,29,31,34,61)])


# look at change in pcover, shoot density, and canopy height in relation to initial
# seagrass % cover, shoot density, canopy height, and growth

# mds of initial conditions
initmds<-metaMDS(init[,c(36,37,38,49)])

mds.scores<-data.frame(scores(initmds,display="sites",choices = c(1,2)))

init2<-bind_cols(init,mds.scores)

initmds

ggplot(data=init2,aes(x=NMDS1,y=NMDS2))+
  geom_point(aes(color=s2.sdens),size=3)+
  scale_color_viridis_c(option="turbo")

# looks like growth and shoot density were correlated in sampling 1
bp+
  geom_point(aes(x=s1grow,y=s1.sdens))

cor(init$s1grow,init$s1.sdens)
cor(init$s2grow,init$s2.sdens)

# initial correlation between growth and shoot density - gone by second sampling
# still there in control plots?

cor(init[init$treat=="UU",]$s1grow,init[init$treat=="UU",]$s1.sdens)
cor(init[init$treat=="UU",]$s2grow,init[init$treat=="UU",]$s2.sdens)

# yes, but does decrease
ci.func<-function(e,se){
  lci<-e-(1.96*se)
  hci<-e+(1.96*se)
  return(paste(round(lci,2), " to ", round(hci,2)))
}
# run model to confirm pattern
lp("lmerTest")
lp("effectsize")
lp("emmeans")

pd.pc<-lmer(pd.pcover~scar*graze+(1|blockID),data=init)
plot(pd.pc)
summary(pd.pc)
effectsize(pd.pc)
emmeans(pd.pc,~graze*scar)
emmeans(pd.pc,~graze)

# grazing treatment negatively influenced the change in % cover
# in control plots thalassia % cover changed by 5.97% (-8.07 to 20.01)
# in grazed plots thalassia % cover decreased by 18.59% (-29.16 to -8.01)
anova(pd.pc)

pd.sd<-lmer(pd.sdens~scar*graze+(1|blockID),data=init)
plot(pd.sd)
summary(pd.sd)
emmeans(pd.sd,~graze*scar)
emmeans(pd.sd,~graze)
# no statistically significant effect on shoot density
# even control plots lost shoot density between s1 and s2: -21.50% (-39.90 to -3.17)
# grazed plots did, on average lose more -32.90 (-47.10 to -18.70)


pd.can<-lmer(pd.can~scar*graze+(1|blockID),data=init)
plot(pd.can)
summary(pd.can)
emmeans(pd.can,~graze*scar)
emmeans(pd.can,~graze)

# grazing had a negative influence on the change in canopy height
# in control plots canopy height increased by 56.42% (30.20 to 82.60)
# in grazed plots canopy height changed by 3.15% (-15.6 to 21.90)

pd.grow<-lmer(pd.grow~scar*graze+(1|blockID),data=init)
plot(pd.grow)
summary(pd.grow)

emmeans(pd.grow,~graze*scar)
emmeans(pd.grow,~graze)

# grazing did not have an effect on growth
# growth rates did not consistently change from sampling 1 to 2 in control plots
# but the average was an increase 14.39% (-24.3 to 53.10)
# while in grazed plots the average was a decrease: -9.19% (-40.01 to 21.70)
pclab<-expression(paste("% change in ",italic("T. testudinum")," cover"))
sdlab<-expression(paste("% change in ",italic("T. testudinum")," density"))
canlab<-expression(paste("% change in ",italic("T. testudinum")," canopy height"))
growlab<-expression(paste("% change in ",italic("T. testudinum")," growth"))


pclab2<-"% change - % cover"
sdlab2<-"% change - density"
canlab2<-"% change - canopy height"
growlab2<-"% change - growth"

# make figures of emmeans for talk
pcem<-data.frame(emmeans(pd.pc,~scar*graze))%>%
  mutate(treatment=case_when(
    scar=="No.scar" & graze=="No.graze"~"Control",
    scar=="Scar" & graze=="No.graze"~"Scar no grazing",
    scar=="No.scar" & graze=="Graze"~"Grazing",
    TRUE~"Scar and grazing"))
pcem$treatment<-factor(pcem$treatment,levels=c("Control",
                                               "Scar no grazing",
                                               "Grazing",
                                               "Scar and grazing"))

ggplot(data=pcem,aes(x=treatment,y=emmean*100,fill=graze))+
  geom_hline(aes(yintercept=0))+
  geom_errorbar(aes(ymin=lower.CL*100,ymax=upper.CL*100),width=.1)+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#fc8d62","#66c2a5"),labels=c("No Grazing","Grazing"),name="Grazing Treatment")+
  xlab("")+
  ylab(pclab)


# see how different raw data is
init.sum<-init%>%
  group_by(scar,graze)%>%
  summarize(pc=mean(pd.pcover),
            pcl=pc-(1.96*(sd(pd.pcover)/sqrt(11))),
            pch=pc+(1.96*(sd(pd.pcover)/sqrt(11))),
            sde=mean(pd.sdens),
            sdl=sde-(1.96*(sd(pd.sdens)/sqrt(11))),
            sdh=sde+(1.96*(sd(pd.sdens)/sqrt(11))),
            can=mean(pd.can),
            canl=can-(1.96*(sd(pd.can)/sqrt(11))),
            canh=can+(1.96*(sd(pd.can)/sqrt(11))),
            grow=mean(pd.grow),
            growl=grow-(1.96*(sd(pd.grow)/sqrt(11))),
            growh=grow+(1.96*(sd(pd.grow)/sqrt(11))))%>%
  mutate(treatment=case_when(
    scar=="No.scar" & graze=="No.graze"~"Control",
    scar=="Scar" & graze=="No.graze"~"Scar",
    scar=="No.scar" & graze=="Graze"~"Grazing",
    TRUE~"Scar and Grazing"))

init.sum$treatment<-factor(init.sum$treatment,levels=c("Control",
                                               "Scar",
                                               "Grazing",
                                               "Scar and Grazing"))

anova(pd.pc)
pc.graze<-expression(paste("F"["1,33"]," = 14.16, p < 0.001"))
(pc.p<-ggplot(data=init.sum,aes(x=treatment,y=pc*100,fill=graze))+
  geom_hline(aes(yintercept=0))+
  geom_errorbar(aes(ymin=pcl*100,ymax=pch*100),width=.1)+
  # geom_bar(stat="identity")+
  geom_point(size=6,pch=21)+
    scale_fill_manual(values=c("#fc8d62","#66c2a5"),labels=c("No Grazing","Grazing"),name="Grazing Treatment")+  xlab("")+
  ylab(pclab2)+
    annotate(geom="text",x=3.5,y=20,label=pc.graze,size=6))
ggsave("figures/initialeffect_pcover.jpg",height=6,width=8)

anova(pd.sd)
sd.graze<-expression(paste("F"["1,33"]," = 0.86, p = 0.36"))
(sd.p<-ggplot(data=init.sum,aes(x=treatment,y=sde*100,fill=graze))+
    geom_hline(aes(yintercept=0))+
    geom_errorbar(aes(ymin=sdl*100,ymax=sdh*100),width=.1)+
    # geom_bar(stat="identity")+
    geom_point(size=6,pch=21)+
    scale_fill_manual(values=c("#fc8d62","#66c2a5"),labels=c("No Grazing","Grazing"),name="Grazing Treatment")+
    xlab("")+
    ylab(sdlab2)+
    annotate(geom="text",x=3.5,y=5,label=sd.graze,size=6))

ggsave("figures/initialeffect_sdens.jpg",height=6,width=8)
anova(pd.can)
can.graze<-expression(paste("F"["1,33"]," = 18.62, p < 0.001"))
(can.p<-ggplot(data=init.sum,aes(x=treatment,y=can*100,fill=graze))+
    geom_hline(aes(yintercept=0))+
    geom_errorbar(aes(ymin=canl*100,ymax=canh*100),width=.1,color="black")+
    #geom_bar(stat="identity")+
    geom_point(size=6,pch=21)+
    scale_fill_manual(values=c("#fc8d62","#66c2a5"),labels=c("No Grazing","Grazing"),name="Grazing Treatment")+
    xlab("")+
    ylab(canlab2)+
    annotate(geom="text",x=3.5,y=88,label=can.graze,size=6))
ggsave("figures/initialeffect_can.jpg",height=6,width=8)


anova(pd.grow)
grow.graze<-expression(paste("F"["1,33"]," = 5.80, p = 0.02"))
(grow.p<-ggplot(data=init.sum,aes(x=treatment,y=grow*100,fill=graze))+
    geom_hline(aes(yintercept=0))+
    geom_errorbar(aes(ymin=growl*100,ymax=growh*100),width=.1,color="black")+
    #geom_bar(stat="identity")+
    geom_point(size=6,pch=21)+
    scale_fill_manual(values=c("#fc8d62","#66c2a5"),labels=c("No Grazing","Grazing"),name="Grazing Treatment")+
    xlab("")+
    ylab(growlab2)+
    annotate(geom="text",x=3.5,y=90,label=grow.graze,size=6))
ggsave("figures/initialeffect_grow.jpg",height=6,width=8)


lp("patchwork")
pc.p+sd.p+can.p+grow.p+plot_layout(guides="collect",axis_titles = "collect")

ggsave("figures/initialresponsetograzing.jpg",width=12,height=10)

# do a linear regression
par(mfrow=c(2,2))
cor(init$s1pn,init$s1OM,use = "complete.obs")

init.lm<-lm(cbind(pd.sdens,pd.pcover,pd.can,pd.grow)~s1pn+s1OM,data=init%>%
                filter(graze=="Graze"))

#plot(init.lm)
summary(init.lm)

ggplot(data=init%>%filter(graze=="Graze"),
       aes(x=s1OM,y=100*pd.grow))+
  geom_hline(aes(yintercept=0))+
  geom_point(size=4)+
  geom_smooth(method="lm",color="black")+
  xlab("Sediment Organic Matter (g /g dw)")+
  ylab(growlab2) 

ggsave("figures/change in grow and om.jpg",height=3,width=4.3)


ggplot(data=init%>%filter(graze=="Graze"),
       aes(x=s1OM,y=100*pd.can))+
  geom_hline(aes(yintercept=0))+
  geom_point(size=4)+
  geom_smooth(method="lm",color="black")+
  xlab("Sediment Organic Matter (g /g dw)")+
  ylab(canlab2) 

ggsave("figures/change in can and om.jpg",height=3,width=4.3)

ggplot(data=init%>%filter(graze=="Graze"),
       aes(x=s1pn,y=100*pd.can))+
  geom_hline(aes(yintercept=0))+
  geom_point(size=4)+
  geom_smooth(method="lm")+
  xlab("%N in leaves")+
  ylab(canlab2) 

ggplot(data=init%>%filter(graze=="Graze"),
       aes(x=s1OM,y=100*pd.sdens))+
  geom_hline(aes(yintercept=0))+
  geom_point(size=4)+
  geom_smooth(method="lm",color="black")+
  xlab("Sediment Organic Matter (g /g dw)")+
  ylab(sdlab2) 

ggsave("figures/change in dens and om.jpg",height=3,width=4.3)

ggplot(data=init%>%filter(graze=="Graze"),
       aes(x=s1pn,y=100*pd.pcover))+
  geom_hline(aes(yintercept=0))+
  geom_point(size=4)+
  geom_smooth(method="lm",color="black")+
  xlab("%N in leaves")+
  ylab(pclab2) 

ggsave("figures/change in pc and pn.jpg",height=3,width=4.3)

