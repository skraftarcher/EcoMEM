# script to evaluate hypothesis 3: 
# lower C:N and C:P ratios in less disturbed sites


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

# load in data

bio<-read.csv("wdata/PROP_sg_lab_combined.csv")%>%
  select(-X)%>%
  filter(bay!="WB")%>%
  mutate(CN=PC/PN,
         CP=PC/PP)

bio$bay<-factor(bio$bay,levels = c("SJB","SAB","NB"))

CN.cnt<-glmmTMB(CN~scar.quantity.100m*bay,
                  data=bio)

glmm.resids(CN.cnt)

CN.cnt2<-glmmTMB(CN~scar.quantity.50m*bay,
                   data=bio)

glmm.resids(CN.cnt2)

CN.cnt3<-glmmTMB(CN~scar.quantity.10m*bay,
                   data=bio)

glmm.resids(CN.cnt3)

CN.cnt4<-glmmTMB(CN~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                     scar.quantity.10m*bay,
                   data=bio)

glmm.resids(CN.cnt4)

CN.cnt5<-glmmTMB(CN~scar.quantity.50m*bay+
                     scar.quantity.10m*bay,
                   data=bio)

glmm.resids(CN.cnt5)

CN.len<-glmmTMB(CN~scar.length.clipped.100m*bay,
                  data=bio)

glmm.resids(CN.len)

CN.len2<-glmmTMB(CN~scar.length.clipped.50m*bay,
                   data=bio)

glmm.resids(CN.len2)


CN.len3<-glmmTMB(CN~scar.length.clipped.10m*bay,
                   data=bio)

glmm.resids(CN.len3)


CN.len4<-glmmTMB(CN~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                     scar.length.clipped.10m*bay,
                   data=bio)

glmm.resids(CN.len4)


CN.len5<-glmmTMB(CN~scar.length.clipped.50m*bay+
                     scar.length.clipped.10m*bay,
                   data=bio)

glmm.resids(CN.len5)

(h3CNaic<-data.frame(model=c("CN.cnt","CN.cnt2","CN.cnt3","CN.cnt4","CN.cnt5",
                                "CN.len","CN.len2","CN.len3","CN.len4","CN.len5"),
                        AICc=c(performance_aicc(CN.cnt),performance_aicc(CN.cnt2),performance_aicc(CN.cnt3),
                               performance_aicc(CN.cnt4),performance_aicc(CN.cnt5),performance_aicc(CN.len),
                               performance_aicc(CN.len2),performance_aicc(CN.len3),performance_aicc(CN.len4),performance_aicc(CN.len5)))%>%
    arrange(AICc)%>%
    mutate(delta.aicc=AICc-AICc[1]))

# six models are equally well supported. see if there is anything significant in any of them
summary(CN.len2)
summary(CN.len3)
summary(CN.cnt3)
summary(CN.cnt2)
summary(CN.cnt)
summary(CN.len)
# only significant relationship is that SJB has a lower C:N ratio than SAB or NB
bio$bay<-relevel(bio$bay,ref="SAB")
summary(update(CN.len2))

# now CP----
CP.CP<-glmmTMB(CP~scar.quantity.100m*bay,
                   data=bio)

glmm.resids(CP.CP)

CP.CP2<-glmmTMB(CP~scar.quantity.50m*bay,
                    data=bio)

glmm.resids(CP.CP2)

CP.CP3<-glmmTMB(CP~scar.quantity.10m*bay,
                    data=bio)

glmm.resids(CP.CP3)

CP.CP4<-glmmTMB(CP~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.quantity.10m*bay,
                    data=bio)

glmm.resids(CP.CP4)


CP.CP5<-glmmTMB(CP~scar.quantity.50m*bay+
                      scar.quantity.10m*bay,
                    data=bio)

glmm.resids(CP.CP5)

CP.len<-glmmTMB(CP~scar.length.clipped.100m*bay,
                   data=bio)

glmm.resids(CP.len)

CP.len2<-glmmTMB(CP~scar.length.clipped.50m*bay,
                    data=bio)

glmm.resids(CP.len2)


CP.len3<-glmmTMB(CP~scar.length.clipped.10m*bay,
                    data=bio)

glmm.resids(CP.len3)


CP.len4<-glmmTMB(CP~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                      scar.length.clipped.10m*bay,
                    data=bio)

glmm.resids(CP.len4)


CP.len5<-glmmTMB(CP~scar.length.clipped.50m*bay+
                      scar.length.clipped.10m*bay,
                    data=bio)

glmm.resids(CP.len5)

(h3CP.aic<-data.frame(model=c("CP.CP","CP.CP2","CP.CP3","CP.CP4","CP.CP5",
                                "CP.len","CP.len2","CP.len3","CP.len4","CP.len5"),
                        AICc=c(performance_aicc(CP.CP),performance_aicc(CP.CP2),performance_aicc(CP.CP3),
                               performance_aicc(CP.CP4),performance_aicc(CP.CP5),performance_aicc(CP.len),
                               performance_aicc(CP.len2),performance_aicc(CP.len3),performance_aicc(CP.len4),
                               performance_aicc(CP.len5)))%>%
    arrange(AICc)%>%
    mutate(delta.aicc=AICc-AICc[1]))

# both count and length at the 10m resolution are the best fit models

summary(CP.len3)
summary(CP.CP3)

# positive, but not significant relationship between CP and scar length in SAB
    # this is significantly different than in NB
bio$bay<-relevel(bio$bay,ref="NB")
summary(update(CP.len3))
summary(update(CP.CP3))
# negative, but not significant relationship between CP and scar length & quantity in NB
bio$bay<-relevel(bio$bay,ref="SJB")
summary(update(CP.len3))
summary(update(CP.CP3))
# significantly lower CP in SJB than in NB, lower than SAB but not significant 


# do we get the same patterns looking at %N and %P
#%N----

PN.cnt<-glmmTMB(PN~scar.quantity.100m*bay,
                  data=bio)

glmm.resids(PN.cnt)

PN.cnt2<-glmmTMB(PN~scar.quantity.50m*bay,
                   data=bio)

glmm.resids(PN.cnt2)

PN.cnt3<-glmmTMB(PN~scar.quantity.10m*bay,
                   data=bio)

glmm.resids(PN.cnt3)

PN.cnt4<-glmmTMB(PN~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                     scar.quantity.10m*bay,
                   data=bio)

glmm.resids(PN.cnt4)

PN.cnt5<-glmmTMB(PN~scar.quantity.50m*bay+
                     scar.quantity.10m*bay,
                   data=bio)

glmm.resids(PN.cnt5)

PN.len<-glmmTMB(PN~scar.length.clipped.100m*bay,
                  data=bio)

glmm.resids(PN.len)

PN.len2<-glmmTMB(PN~scar.length.clipped.50m*bay,
                   data=bio)

glmm.resids(PN.len2)


PN.len3<-glmmTMB(PN~scar.length.clipped.10m*bay,
                   data=bio)

glmm.resids(PN.len3)


PN.len4<-glmmTMB(PN~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                     scar.length.clipped.10m*bay,
                   data=bio)

glmm.resids(PN.len4)


PN.len5<-glmmTMB(PN~scar.length.clipped.50m*bay+
                     scar.length.clipped.10m*bay,
                   data=bio)

glmm.resids(PN.len5)

(h3PNaic<-data.frame(model=c("PN.cnt","PN.cnt2","PN.cnt3","PN.cnt4","PN.cnt5",
                                "PN.len","PN.len2","PN.len3","PN.len4","PN.len5"),
                        AICc=c(performance_aicc(PN.cnt),performance_aicc(PN.cnt2),performance_aicc(PN.cnt3),
                               performance_aicc(PN.cnt4),performance_aicc(PN.cnt5),performance_aicc(PN.len),
                               performance_aicc(PN.len2),performance_aicc(PN.len3),performance_aicc(PN.len4),performance_aicc(PN.len5)))%>%
    arrange(AICc)%>%
    mutate(delta.aicc=AICc-AICc[1]))


# 6 equally well supported models - just like CN
summary(PN.cnt2)
summary(PN.len2)
summary(PN.len3)
summary(PN.cnt3)
summary(PN.cnt)
summary(PN.len)
# nothing significant, but bay is close. Just out of curiosity - when doing an ANOVA with bay only?
PN.aov<-aov(PN~bay,data=bio)
summary(PN.aov)
# not significant

# %P----

PP.cnt<-glmmTMB(PP~scar.quantity.100m*bay,
                data=bio)

glmm.resids(PP.cnt)

PP.cnt2<-glmmTMB(PP~scar.quantity.50m*bay,
                 data=bio)

glmm.resids(PP.cnt2)

PP.cnt3<-glmmTMB(PP~scar.quantity.10m*bay,
                 data=bio)

glmm.resids(PP.cnt3)

PP.cnt4<-glmmTMB(PP~scar.quantity.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                   scar.quantity.10m*bay,
                 data=bio)

glmm.resids(PP.cnt4)

PP.cnt5<-glmmTMB(PP~scar.quantity.50m*bay+
                   scar.quantity.10m*bay,
                 data=bio)

glmm.resids(PP.cnt5)

PP.len<-glmmTMB(PP~scar.length.clipped.100m*bay,
                data=bio)

glmm.resids(PP.len)

PP.len2<-glmmTMB(PP~scar.length.clipped.50m*bay,
                 data=bio)

glmm.resids(PP.len2)


PP.len3<-glmmTMB(PP~scar.length.clipped.10m*bay,
                 data=bio)

glmm.resids(PP.len3)


PP.len4<-glmmTMB(PP~scar.length.clipped.100m*bay+# including the both the 100 and 50m radius introduced colinearity 
                   scar.length.clipped.10m*bay,
                 data=bio)

glmm.resids(PP.len4)


PP.len5<-glmmTMB(PP~scar.length.clipped.50m*bay+
                   scar.length.clipped.10m*bay,
                 data=bio)

glmm.resids(PP.len5)

(h3PPaic<-data.frame(model=c("PP.cnt","PP.cnt2","PP.cnt3","PP.cnt4","PP.cnt5",
                             "PP.len","PP.len2","PP.len3","PP.len4","PP.len5"),
                     AICc=c(performance_aicc(PP.cnt),performance_aicc(PP.cnt2),performance_aicc(PP.cnt3),
                            performance_aicc(PP.cnt4),performance_aicc(PP.cnt5),performance_aicc(PP.len),
                            performance_aicc(PP.len2),performance_aicc(PP.len3),performance_aicc(PP.len4),performance_aicc(PP.len5)))%>%
    arrange(AICc)%>%
    mutate(delta.aicc=AICc-AICc[1]))

# top twp models are scar quantity and length at 10m

summary(PP.len3)
summary(PP.cnt3)
