# Script to address hypothesis 4: 
# More even scarring = lower density of seagrass

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
sg.prop<-read.csv("wdata/PROP_combined_sg_prop.csv")%>%
  mutate(total.shtd=sht.d.m_H+sht.d.m_He+sht.d.m_S+sht.d.m_Tt)
sg.prop$bay<-factor(sg.prop$bay,levels = c("NB","SJB","SAB","WB"))


# thalassia shoot density----
Ttsd.dist<-glmmTMB(sht.d.m_Tt~ave.distance*bay,
                  family=tweedie,
                  data=sg.prop)

glmm.resids(Ttsd.dist)

summary(Ttsd.dist)
# nothing is significant

# look at total seagrass density

Totsd.dist<-glmmTMB(total.shtd~ave.distance*bay,
                  family=tweedie,
                  data=sg.prop)

glmm.resids(Totsd.dist)

summary(Totsd.dist)
# there are some significant interactions here
# in North Bay total seargass density goes up with increasing distance between scars
# significantly different than SAB

sg.prop$bay<-relevel(sg.prop$bay,ref="SJB")
summary(update(Totsd.dist))

sg.prop$bay<-relevel(sg.prop$bay,ref="SAB")
summary(update(Totsd.dist))


sg.prop$bay<-relevel(sg.prop$bay,ref="WB")
summary(update(Totsd.dist))


# only significant pattern is that in North Bay as scars are more evenly spread out total seagrass density goes up.