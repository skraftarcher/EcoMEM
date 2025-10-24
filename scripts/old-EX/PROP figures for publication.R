# make figures for prop scar paper

# load custom script to script to check if a package is installed, install it if necessary, then load it
source("scripts/install_packages_function.R")
source("scripts/glmmtmb resids.R")
# load the packages we need for this script
lp("tidyverse")
lp("lmerTest")
lp("ggeffects")
lp("glmmTMB")
lp("performance")
lp("patchwork")

# set some figure defaults
theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           axis.text = element_text(size=12),
                           axis.title = element_text(size=14)))


#hypothesis 1----

# load the data set - we didn't see halodule in SJB so leaving it out of these models
sg.prop<-read.csv("wdata/PROP_combined_sg_prop.csv")

sg.prop.figs<-read.csv("wdata/PROP_combined_sg_prop.csv")%>%
  pivot_longer(scar.quantity.100m:scar.length.clipped.10m,names_to="measure",values_to = "values")%>%
  mutate(measure.label=case_when(
    measure=="scar.quantity.100m"~"Scars within 100m radius",
    measure=="scar.quantity.10m"~"Scars within 10m radius",
    measure=="scar.quantity.50m"~"Scars within 50m radius",
    measure=="scar.length.clipped.100m"~"Scar length within 100m radius",
    measure=="scar.length.clipped.10m"~"Scar length within 10m radius",
    measure=="scar.length.clipped.50m"~"Scar length within 50m radius"),
    bay.label=case_when(
      bay=="NB"~"North Bay",
      bay=="WB"~"West Bay",
      bay=="SAB"~"St. Andrew Bay",
      bay=="SJB"~"St. Joseph Bay"))

sg.prop$bay<-factor(sg.prop$bay,levels=c("NB","WB","SAB","SJB"))

sg.proph1<-filter(sg.prop,bay!="SJB")
sg.proph1.figs<-filter(sg.prop.figs,bay!="SJB")


halsd.cnt4<-glmmTMB(sht.d.m_H~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.quantity.10m*bay,
                    data=sg.proph1,
                    family=tweedie())

h1.pred100<-ggpredict(halsd.cnt4,terms = c("scar.quantity.100m","bay"))
h1.pred100<-data.frame(h1.pred100)%>%
  rename("bay"=group)%>%
  mutate(measure.label="Scars within 100m radius",
         bay.label=case_when(
           bay=="NB"~"North Bay",
           bay=="WB"~"West Bay",
           bay=="SAB"~"St. Andrew Bay",
           bay=="SJB"~"St. Joseph Bay"))

h1.pred10<-ggpredict(halsd.cnt4,terms = c("scar.quantity.10m","bay"))
h1.pred10<-data.frame(h1.pred10)%>%
  rename("bay"=group)%>%
  mutate(measure.label="Scars within 10m radius",
         bay.label=case_when(
           bay=="NB"~"North Bay",
           bay=="WB"~"West Bay",
           bay=="SAB"~"St. Andrew Bay",
           bay=="SJB"~"St. Joseph Bay"))

h1.preds<-bind_rows(data.frame(h1.pred100),data.frame(h1.pred10))%>%
  rename("Bay"=bay)

h1.preds.nbwb.100<-h1.preds%>%
  filter(Bay!="SAB")%>%
  filter(x<=42)%>%
  filter(measure.label %in% c("Scars within 100m radius"))


h1.preds.sab<-h1.preds%>%
  filter(Bay=="SAB")%>%
  filter(measure.label %in% c("Scars within 100m radius"))

h1.preds.10<-h1.preds%>%
  filter(measure.label %in% c("Scars within 10m radius"))%>%
  mutate(filt.if=case_when(
    Bay=="SAB" & conf.high <5000~1,
    Bay!="SAB" & x <=4~1,
    TRUE~0
  ))%>%
  filter(filt.if==1)%>%
  mutate(conf.high=ifelse(conf.high>=5000,5000,conf.high),
         predicted=ifelse(predicted>=5000,5000,predicted))

h1.preds.10nbwb<-h1.preds.10%>%
  filter(Bay!="SAB")
h1.preds.10sab<-h1.preds.10%>%
  filter(Bay=="SAB")

hal.sht<-expression(paste(italic("H. wrightii")," shoots m"^2))
count.100<-"Propellor scars within 100m radius"
count.10<-"Propellor scars within 10m radius"

# for nb and wb
options(ggplot2.discrete.colour= c("#102933","#855fef"),
        ggplot2.discrete.fill= c("#102933","#855fef"))

# halodule v scars 
(h1.nbwb.100<-ggplot(data=sg.proph1.figs%>%
                  filter(measure %in% c("scar.quantity.100m"))%>%
              rename("Bay"=bay)%>%
                filter(Bay !="SAB"))+
  geom_point(aes(x=values,y=sht.d.m_H,color=Bay),size=3,alpha=.6)+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=Bay),
                alpha=.2,data=h1.preds.nbwb.100)+
    geom_line(aes(x=x,y=predicted,color=Bay),data=h1.preds.nbwb.100,linewidth=1.25)+
    ylab(hal.sht)+
    xlab("Count of Propellor Scars")+
    theme(legend.position = "none")+
    facet_wrap(~measure.label,scales="free"))


# for sab 
# halodule v scars SAB 100
(h1.sab100<-ggplot(data=sg.proph1.figs%>%
              filter(measure %in% c("scar.quantity.100m"))%>%
              rename("Bay"=bay)%>%
                filter(Bay=="SAB"))+
    geom_point(aes(x=values,y=sht.d.m_H),,color="#61a1c1",size=3,alpha=.6)+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),
                alpha=.2,data=h1.preds.sab,fill="#61a1c1")+
    geom_line(aes(x=x,y=predicted),color="#61a1c1",data=h1.preds.sab,linewidth=1.25)+
    ylab(hal.sht)+
    xlab("Count of Propellor Scars")+
    theme(legend.position = "none"))

# hal v scar 10m
(h1.nbwb.10<-ggplot(data=sg.proph1.figs%>%
                       filter(measure %in% c("scar.quantity.10m"))%>%
                       rename("Bay"=bay)%>%
                       filter(Bay !="SAB"))+
    geom_point(aes(x=values,y=sht.d.m_H,color=Bay),size=3,alpha=.6)+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=Bay),
                alpha=.2,data=h1.preds.10nbwb)+
    geom_line(aes(x=x,y=predicted,color=Bay),data=h1.preds.10nbwb,linewidth=1.25)+
    ylab(hal.sht)+
    xlab("Count of Propellor Scars")+
    facet_wrap(~measure.label,scales="free"))


# for sab 
# halodule v scars SAB 10
(h1.sab10<-ggplot(data=sg.proph1.figs%>%
                     filter(measure %in% c("scar.quantity.10m"))%>%
                     rename("Bay"=bay)%>%
                     filter(Bay=="SAB"))+
    geom_point(aes(x=values,y=sht.d.m_H,color=Bay),size=3,alpha=.6)+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=Bay),
                alpha=.2,data=h1.preds.10sab,)+
    geom_line(aes(x=x,y=predicted,color=Bay),data=h1.preds.10sab,linewidth=1.25)+
    ylab(hal.sht)+
    scale_color_manual(values="#61a1c1")+
    scale_fill_manual(values="#61a1c1")+
    xlab("Count of Propellor Scars"))

(h1.nbwb.100/h1.sab100)+ plot_layout(axes="collect")|(h1.nbwb.10/h1.sab10)+ plot_layout(axes="collect")
ggsave("figures/PROP_h1.png",height=5,width=10)

# hypothesis 2

Ttsd.cnt<-glmmTMB(sht.d.m_Tt~scar.quantity.100m*bay,
                  data=sg.prop,
                  family=tweedie())

h2.pred<-ggpredict(Ttsd.cnt,terms = c("scar.quantity.100m","bay"))
plot(h2.pred)

h2.pred<-data.frame(h2.pred)%>%
  rename("Bay"=group)%>%
  mutate(measure.label="Scars within 100m radius",
         bay.label=case_when(
           Bay=="NB"~"North Bay",
           Bay=="WB"~"West Bay",
           Bay=="SAB"~"St. Andrew Bay",
           Bay=="SJB"~"St. Joseph Bay"))

h2.prednbwb<-h2.pred%>%
  filter(Bay %in% c("NB","WB"))%>%
  filter(x<=42)
h2.predsjbsab<-h2.pred%>%
  filter(!Bay %in% c("NB","WB"))

sg.prop.figs<-sg.prop.figs%>%
  rename("Bay"=bay)
tt.sht<-expression(paste(italic("T. testudinum")," shoots m"^2))


# for nb and wb

(h2<-ggplot(data=sg.prop.figs%>%
                  filter(measure %in% c("scar.quantity.100m"))%>%
              filter(Bay %in% c("NB","WB")))+
    geom_point(aes(x=values,y=sht.d.m_H,color=Bay),size=3,alpha=.6)+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=Bay),alpha=.2,data=h2.prednbwb)+
    geom_line(aes(x=x,y=predicted,color=Bay),data=h2.prednbwb,linewidth=1.25)+
    ylab(tt.sht)+
    xlab(count.100))

# for sab and sjb

(h2.sjbsab<-ggplot(data=sg.prop.figs%>%
              filter(measure %in% c("scar.quantity.100m"))%>%
              filter(!Bay %in% c("NB","WB")))+
    geom_point(aes(x=values,y=sht.d.m_H,color=Bay),size=3,alpha=.6)+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=Bay),alpha=.2,data=h2.predsjbsab)+
    geom_line(aes(x=x,y=predicted,color=Bay),data=h2.predsjbsab,linewidth=1.25)+
    ylab(tt.sht)+
    scale_color_manual(values=c("#61a1c1","#73dde0"))+
    scale_fill_manual(values=c("#61a1c1","#73dde0"))+
    xlab(count.100))

h2/h2.sjbsab+plot_layout(axes="collect")
ggsave("figures/PROP_h2.png",height=5,width=5)

# hypothesis 3 ----
bio<-read.csv("wdata/PROP_sg_lab_combined.csv")%>%
  select(-X)%>%
  filter(bay!="WB")

bio$bay<-factor(bio$bay,levels = c("SJB","SAB","NB"))


bio$bay<-factor(bio$bay,levels=c("NB","WB","SAB","SJB"))

options(ggplot2.discrete.colour= c("#102933","#61a1c1","#73dde0"),
        ggplot2.discrete.fill= c("#102933","#61a1c1","#73dde0"))

bio.len<-glmmTMB(Tt.biom~scar.length.clipped.100m*bay,
                 data=bio)
h3.pred<-ggpredict(bio.len,terms = c("scar.length.clipped.100m","bay"))
plot(h3.pred)

h3.pred<-data.frame(h3.pred)%>%
  rename("Bay"=group)%>%
  mutate(measure.label="Scar length within 100m radius",
         bay.label=case_when(
           Bay=="NB"~"North Bay",
           Bay=="SAB"~"St. Andrew Bay",
           Bay=="SJB"~"St. Joseph Bay"))

tt.bio<-expression(paste("Aboveground ",italic("T. testudinum")," biomass (g m"^"2", ")"))

h3.pred.sab<-h3.pred%>%
  filter(Bay=="SAB")

h3.pred.nbsjb<-h3.pred%>%
  filter(Bay!="SAB")%>%
  filter(x<=320)

(h3<-ggplot(data=bio%>%
             rename("Bay"=bay)%>%
              filter(Bay!="SAB"))+
  geom_point(aes(x=scar.length.clipped.100m,y=Tt.biom,color=Bay),size=3,alpha=.6)+
  geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=Bay),
              alpha=.2,data=h3.pred.nbsjb)+
  geom_line(aes(x=x,y=predicted,color=Bay),data=h3.pred.nbsjb,linewidth=1.25)+
  ylab(tt.bio)+
    scale_color_manual(values=c("#102933","#73dde0"))+
    scale_fill_manual(values=c("#102933","#73dde0"))+
  xlab("Total scar length within 100m radius"))

(h3.sab<-ggplot(data=bio%>%
              rename("Bay"=bay)%>%
              filter(Bay=="SAB"))+
    geom_point(aes(x=scar.length.clipped.100m,y=Tt.biom,color=Bay),size=3,alpha=.6)+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,fill=Bay),
                alpha=.2,data=h3.pred.sab)+
    geom_line(aes(x=x,y=predicted,color=Bay),data=h3.pred.sab,linewidth=1.25)+
    ylab(tt.bio)+
    scale_color_manual(values=c("#61a1c1"))+
    scale_fill_manual(values=c("#61a1c1"))+
    xlab("Total scar length within 100m radius"))

h3/h3.sab+plot_layout(axes="collect")


ggsave("figures/PROP_h3.png",height=5,width=5)
