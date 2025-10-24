# Script to address hypothesis 1: 
# Halodule wrightii will be more abundant/make up more of the community at more disturbed sites

# load custom script to script to check if a package is installed, install it if necessary, then load it
source("scripts/install_packages_function.R")
source("scripts/glmmtmb resids.R")
# load the packages we need for this script
lp("tidyverse")
lp("lmerTest")
lp("ggeffects")
lp("glmmTMB")
lp("performance")

# load the data set - we didn't see halodule in SJB so leaving it out of these models
sg.prop<-read.csv("wdata/PROP_combined_sg_prop.csv")%>%
  filter(bay!="SJB")

# remind myself of the organization of the data
glimpse(sg.prop)

# try this the most straight forward ways first
# as much in 1 model as possible ----
#shoot density----
halsd.cnt<-glmmTMB(sht.d.m_H~scar.quantity.100m*bay,
                     data=sg.prop,
                     family=tweedie())

glmm.resids(halsd.cnt)

halsd.cnt2<-glmmTMB(sht.d.m_H~scar.quantity.50m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(halsd.cnt2)

halsd.cnt3<-glmmTMB(sht.d.m_H~scar.quantity.10m*bay,
                      data=sg.prop,
                    family=tweedie())

glmm.resids(halsd.cnt3)

halsd.cnt4<-glmmTMB(sht.d.m_H~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                   scar.quantity.10m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(halsd.cnt4)

halsd.cnt5<-glmmTMB(sht.d.m_H~scar.quantity.50m*bay+
                      scar.quantity.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halsd.cnt5)

halsd.len<-glmmTMB(sht.d.m_H~scar.length.clipped.100m*bay,
                   data=sg.prop,
                   family=tweedie)

glmm.resids(halsd.len)

halsd.len2<-glmmTMB(sht.d.m_H~scar.length.clipped.50m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halsd.len2)


halsd.len3<-glmmTMB(sht.d.m_H~scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halsd.len3)


halsd.len4<-glmmTMB(sht.d.m_H~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halsd.len4)


halsd.len5<-glmmTMB(sht.d.m_H~scar.length.clipped.50m*bay+
                      scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halsd.len5)

(h1shtd.aic<-data.frame(model=c("halsd.cnt","halsd.cnt2","halsd.cnt3","halsd.cnt4","halsd.cnt5",
                                "halsd.len","halsd.len2","halsd.len3","halsd.len4","halsd.len5"),
                        AICc=c(performance_aicc(halsd.cnt),performance_aicc(halsd.cnt2),performance_aicc(halsd.cnt3),
                               performance_aicc(halsd.cnt4),performance_aicc(halsd.cnt5),performance_aicc(halsd.len),
                               performance_aicc(halsd.len2),performance_aicc(halsd.len3),performance_aicc(halsd.len4),performance_aicc(halsd.len5)))%>%
    arrange(AICc)%>%
    mutate(delta.aicc=AICc-AICc[1]))



# best fit model is halsd.cnt4
summary(halsd.cnt4)
# this is the model that includes scar quantity at the 100 and 10 m scales
# the only thing that is significant is baySAB:scar.quantity.10m

#percent cover----
halpc.cnt<-glmmTMB(pc.m_H~scar.quantity.100m*bay,
                   data=sg.prop,
                   family=tweedie())

glmm.resids(halpc.cnt)

halpc.cnt2<-glmmTMB(pc.m_H~scar.quantity.50m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.cnt2)

halpc.cnt3<-glmmTMB(pc.m_H~scar.quantity.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.cnt3)

halpc.cnt4<-glmmTMB(pc.m_H~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.quantity.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.cnt4)

halpc.cnt5<-glmmTMB(pc.m_H~scar.quantity.50m*bay+
                      scar.quantity.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.cnt5)

halpc.len<-glmmTMB(pc.m_H~scar.length.clipped.100m*bay,
                   data=sg.prop,
                   family=tweedie)

glmm.resids(halpc.len)

halpc.len2<-glmmTMB(pc.m_H~scar.length.clipped.50m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.len2)


halpc.len3<-glmmTMB(pc.m_H~scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.len3)


halpc.len4<-glmmTMB(pc.m_H~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.len4)


halpc.len5<-glmmTMB(pc.m_H~scar.length.clipped.50m*bay+
                      scar.length.clipped.10m*bay,
                    data=sg.prop,
                    family=tweedie())

glmm.resids(halpc.len5)

(h1pc.aic<-data.frame(model=c("halpc.cnt","halpc.cnt2","halpc.cnt3","halpc.cnt4","halpc.cnt5",
                                "halpc.len","halpc.len2","halpc.len3","halpc.len4","halpc.len5"),
                        AICc=c(performance_aicc(halpc.cnt),performance_aicc(halpc.cnt2),performance_aicc(halpc.cnt3),
                               performance_aicc(halpc.cnt4),performance_aicc(halpc.cnt5),performance_aicc(halpc.len),
                               performance_aicc(halpc.len2),performance_aicc(halpc.len3),performance_aicc(halpc.len4),performance_aicc(halpc.len5)))%>%
    arrange(AICc))


#the same model is the best fit model with percent cover - halpc.cnt4
summary(halpc.cnt4)
# same story on output too

# same - count and shoot density or pc still the most interesting models. They say the same thing.
# remind myself of the results
summary(halsd.cnt4)
summary(halpc.cnt4)

# update with SAB as the reference bay

sg.prop$bay<-factor(sg.prop$bay,levels = c("SAB","WB","NB"))

summary(update(halsd.cnt4))
sg.prop$bay<-relevel(sg.prop$bay,ref="WB")
summary(update(halsd.cnt4))

# reminder - I excluded SJB from this analysis because we didn't record any Halodule at the sites included in the analysis.
# overall - model with scar counts at 10 and 100 m the best fit model whether looking at shoot density or percent cover.
# At the larger landscape scale (100m) there is a negative relationship between halodule density and scar count - i.e., 
# more scars = less halodule (this is not significant in SAB or NB, but is in WB)
# at the 10m scale it depends on which bay you are in. 
# in North Bay there is a non-significant positive relationship between scar count and halodule density
# in SAB there is a significant negative relationship between halodule densities and scar count
    # this is significantly different than the relationship between scar count and haldoule in NB and WB
# in West Bay there is a significant positive relationship between scar count and halodule density.

# to put these results in context:
theme_set(theme_bw()+theme(panel.grid = element_blank(),axis.title.y = element_text(size=16)))

ggplot(data=sg.prop)+
  geom_boxplot(aes(y=scar.quantity.100m,fill=bay))


ggplot(data=sg.prop)+
  geom_boxplot(aes(y=scar.quantity.10m,fill=bay))

ggplot(data=sg.prop)+
  geom_boxplot(aes(y=sht.d.m_H,fill=bay))

ggplot(data=sg.prop)+
  geom_point(aes(y=sht.d.m_H,x=scar.quantity.100m,color=bay))+
  geom_smooth(aes(y=sht.d.m_H,x=scar.quantity.100m,color=bay),method="lm")+
  facet_wrap(~bay,scales="free")

ggplot(data=sg.prop)+
  geom_point(aes(y=sht.d.m_H,x=scar.quantity.10m,color=bay))+
  geom_smooth(aes(y=sht.d.m_H,x=scar.quantity.10m,color=bay),method="lm")+
  facet_wrap(~bay,scales="free")

# conclusion for me - these relationships are driven by a few points, especially at the 10m resolution
