# Script to address hypothesis 2: 
# More disturbance = lower density of T. testudinum (or all seagrass?)

# load custom script to script to check if a package is installed, install it if necessary, then load it
source("scripts/install_packages_function.R")
source("scripts/glmmtmb resids.R")
# load the packages we need for this script
lp("tidyverse")
lp("lmerTest")
lp("olsrr")
lp("ggeffects")
lp("performance")
lp("glmmTMB")

# load the data set
sg.prop<-read.csv("wdata/PROP_combined_sg_prop.csv")
# calculate additional variable which is total seagrass density
sg.prop<-sg.prop%>%
  mutate(total.shtd=sht.d.m_H+sht.d.m_He+sht.d.m_S+sht.d.m_Tt)

# start with only Thalassia - use same model as H1-----

#shoot density----
Ttsd.cnt<-glmmTMB(sht.d.m_Tt~scar.quantity.100m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(Ttsd.cnt)

Ttsd.cnt2<-glmmTMB(sht.d.m_Tt~scar.quantity.50m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.cnt2)

Ttsd.cnt3<-glmmTMB(sht.d.m_Tt~scar.quantity.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.cnt3)

Ttsd.cnt4<-glmmTMB(sht.d.m_Tt~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.quantity.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.cnt4)

Ttsd.cnt5<-glmmTMB(sht.d.m_Tt~scar.quantity.50m*bay+
                      scar.quantity.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.cnt5)

Ttsd.len<-glmmTMB(sht.d.m_Tt~scar.length.clipped.100m*bay,
                   data=sg.prop,
                   family=tweedie)

glmm.resids(Ttsd.len)

Ttsd.len2<-glmmTMB(sht.d.m_Tt~scar.length.clipped.50m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.len2)


Ttsd.len3<-glmmTMB(sht.d.m_Tt~scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.len3)


Ttsd.len4<-glmmTMB(sht.d.m_Tt~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.len4)


Ttsd.len5<-glmmTMB(sht.d.m_Tt~scar.length.clipped.50m*bay+
                      scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(Ttsd.len5)

(h2shtd.aic<-data.frame(model=c("Ttsd.cnt","Ttsd.cnt2","Ttsd.cnt3","Ttsd.cnt4","Ttsd.cnt5",
                               "Ttsd.len","Ttsd.len2","Ttsd.len3","Ttsd.len4","Ttsd.len5"),
                        AICc=c(performance_aicc(Ttsd.cnt),performance_aicc(Ttsd.cnt2),performance_aicc(Ttsd.cnt3),
                               performance_aicc(Ttsd.cnt4),performance_aicc(Ttsd.cnt5),performance_aicc(Ttsd.len),
                               performance_aicc(Ttsd.len2),performance_aicc(Ttsd.len3),performance_aicc(Ttsd.len4),performance_aicc(Ttsd.len5)))%>%
    arrange(AICc)%>%
    mutate(delta.aicc=AICc-AICc[1]))

# the 100m scar count model is the best fit
sg.prop$bay<-factor(sg.prop$bay,levels = c("NB","SJB","SAB","WB"))
summary(update(Ttsd.cnt))
# there is a positive relationship between scar quantity at 100m and Thalassia shoot density in North bay
# this relationship is different than all other bays
sg.prop$bay<-relevel(sg.prop$bay,ref="SJB")
summary(update(Ttsd.cnt))
# there is no significant relationship between scar quantity and Thalassia density in SJB
sg.prop$bay<-relevel(sg.prop$bay,ref="WB")
summary(update(Ttsd.cnt))
# also no significant relationship in West Bay
sg.prop$bay<-relevel(sg.prop$bay,ref="SAB")
summary(update(Ttsd.cnt))
#there is a positive relationship in SAB between Thalassia density and scar quantity


# to put these results in context:
theme_set(theme_bw()+theme(panel.grid = element_blank(),axis.title.y = element_text(size=16)))

ggplot(data=sg.prop)+
  geom_boxplot(aes(y=scar.quantity.100m,fill=bay))


ggplot(data=sg.prop)+
  geom_boxplot(aes(y=sht.d.m_Tt,fill=bay))

ggplot(data=sg.prop)+
  geom_point(aes(y=sht.d.m_Tt,x=scar.quantity.100m,color=bay))+
  geom_smooth(aes(y=sht.d.m_Tt,x=scar.quantity.100m,color=bay),method="lm")+
  facet_wrap(~bay,scales="free")

# this relationship looks much more robust than the halodule one, driven by more data points and smaller error in SAB and NB


# now look at this for total seagrass----

#shoot density----
totsd.cnt<-glmmTMB(total.shtd~scar.quantity.100m*bay,
                  data=sg.prop,
                  family=tweedie())

glmm.resids(totsd.cnt)

totsd.cnt2<-glmmTMB(total.shtd~scar.quantity.50m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.cnt2)

totsd.cnt3<-glmmTMB(total.shtd~scar.quantity.10m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.cnt3)

totsd.cnt4<-glmmTMB(total.shtd~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                     scar.quantity.10m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.cnt4)

totsd.cnt5<-glmmTMB(total.shtd~scar.quantity.50m*bay+
                     scar.quantity.10m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.cnt5)

totsd.len<-glmmTMB(total.shtd~scar.length.clipped.100m*bay,
                  data=sg.prop,
                  family=tweedie)

glmm.resids(totsd.len)

totsd.len2<-glmmTMB(total.shtd~scar.length.clipped.50m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.len2)


totsd.len3<-glmmTMB(total.shtd~scar.length.clipped.10m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.len3)


totsd.len4<-glmmTMB(total.shtd~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                     scar.length.clipped.10m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.len4)


totsd.len5<-glmmTMB(total.shtd~scar.length.clipped.50m*bay+
                     scar.length.clipped.10m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(totsd.len5)

(h2shtdtot.aic<-data.frame(model=c("totsd.cnt","totsd.cnt2","totsd.cnt3","totsd.cnt4","totsd.cnt5",
                                "totsd.len","totsd.len2","totsd.len3","totsd.len4","totsd.len5"),
                        AICc=c(performance_aicc(totsd.cnt),performance_aicc(totsd.cnt2),performance_aicc(totsd.cnt3),
                               performance_aicc(totsd.cnt4),performance_aicc(totsd.cnt5),performance_aicc(totsd.len),
                               performance_aicc(totsd.len2),performance_aicc(totsd.len3),performance_aicc(totsd.len4),performance_aicc(totsd.len5)))%>%
    arrange(AICc))


# two best fit models - one with scar count and one with scar length at 100m
# lets look and see if they say the same thing
summary(totsd.cnt)
summary(totsd.len)
# yes they do. 
# to be consistent with other models lets look at count in more depth
summary(totsd.cnt)
# positive relationship between scar count and total seagrass density but not significant in SAB
# this is significantly different than west bay

sg.prop$bay<-relevel(sg.prop$bay,ref="SJB")
summary(update(totsd.cnt))
# there is no significant relationship between scar quantity and seagrass density in SJB
sg.prop$bay<-relevel(sg.prop$bay,ref="WB")
summary(update(totsd.cnt))
# There is a significant negative relationship between scar quantity and total seagrass density in west bay
# this is different than every other bay
sg.prop$bay<-relevel(sg.prop$bay,ref="NB")
summary(update(totsd.cnt))
# no significant relationship in North bay


theme_set(theme_bw()+theme(panel.grid = element_blank(),axis.title.y = element_text(size=16)))

ggplot(data=sg.prop)+
  geom_boxplot(aes(y=scar.quantity.100m,fill=bay))


ggplot(data=sg.prop)+
  geom_boxplot(aes(y=total.shtd,fill=bay))

ggplot(data=sg.prop)+
  geom_point(aes(y=total.shtd,x=scar.quantity.100m,color=bay))+
  geom_smooth(aes(y=total.shtd,x=scar.quantity.100m,color=bay),method="lm")+
  facet_wrap(~bay,scales="free")


# biomass----
bio<-read.csv("wdata/PROP_sg_lab_combined.csv")%>%
  select(-X)%>%
  filter(bay!="WB")# don't have enough cores from WB to include

table(bio$bay)

bio.cnt<-gbaybio.cnt<-glmmTMB(Tt.biom~scar.quantity.100m*bay,
                   data=bio)

glmm.resids(bio.cnt)

bio.cnt2<-glmmTMB(Tt.biom~scar.quantity.50m*bay,
                    data=bio)

glmm.resids(bio.cnt2)

bio.cnt3<-glmmTMB(Tt.biom~scar.quantity.10m*bay,
                    data=bio)

glmm.resids(bio.cnt3)

bio.cnt4<-glmmTMB(Tt.biom~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.quantity.10m*bay,
                    data=bio)

glmm.resids(bio.cnt4)

bio.cnt5<-glmmTMB(Tt.biom~scar.quantity.50m*bay+
                      scar.quantity.10m*bay,
                    data=bio)

glmm.resids(bio.cnt5)

bio.len<-glmmTMB(Tt.biom~scar.length.clipped.100m*bay,
                   data=bio)

glmm.resids(bio.len)

bio.len2<-glmmTMB(Tt.biom~scar.length.clipped.50m*bay,
                    data=bio)

glmm.resids(bio.len2)


bio.len3<-glmmTMB(Tt.biom~scar.length.clipped.10m*bay,
                    data=bio)

glmm.resids(bio.len3)


bio.len4<-glmmTMB(Tt.biom~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.length.clipped.10m*bay,
                    data=bio)

glmm.resids(bio.len4)


bio.len5<-glmmTMB(Tt.biom~scar.length.clipped.50m*bay+
                      scar.length.clipped.10m*bay,
                    data=bio)

glmm.resids(bio.len5)

(h2bio.aic<-data.frame(model=c("bio.cnt","bio.cnt2","bio.cnt3","bio.cnt4","bio.cnt5",
                                "bio.len","bio.len2","bio.len3","bio.len4","bio.len5"),
                        AICc=c(performance_aicc(bio.cnt),performance_aicc(bio.cnt2),performance_aicc(bio.cnt3),
                               performance_aicc(bio.cnt4),performance_aicc(bio.cnt5),performance_aicc(bio.len),
                               performance_aicc(bio.len2),performance_aicc(bio.len3),performance_aicc(bio.len4),performance_aicc(bio.len5)))%>%
    arrange(AICc)%>%
    mutate(delta.aicc=AICc-AICc[1]))

# scar length and sound at 100m as well as count at 10m are all equally well supported
bio$bay<-factor(bio$bay,levels = c("NB","SJB","SAB"))
summary(bio.len)
summary(bio.cnt)
summary(bio.cnt3)



# only significant differences are by bay
bio$bay<-relevel(bio$bay,ref="SJB")
summary(update(bio.len))
summary(update(bio.cnt))
summary(update(bio.cnt3))
# negative relationship between biomass and scar length at 100m in SJB 
    #This is different than SAB
# negative, but not significant, relationship between biomass and scar quantity at 100m in SJB
# negative, but not significant, relationship between biomass and scar quantity at 10m in SJB



bio$bay<-relevel(bio$bay,ref="SAB")
summary(update(bio.len))
summary(update(bio.cnt))
summary(update(bio.cnt3))
# only significant differences are by bay

