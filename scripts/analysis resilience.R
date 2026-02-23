# analyze resilience dataset

source("scripts/install_packages_function.R")

lp("tidyverse")
lp("vegan")
lp("ggeffects")
lp("lmerTest")
res.dat<-read.csv("wdata/resilience and sulfide.csv")
par(mfrow=c(2,2))

# first, only look at grazed plots
pairs(res.dat[,-1:-9])
pc.res<-lmer(pcover~root.d34S*graze+OMg.pergdw*graze+(1|blockID),data=res.dat)

plot(pc.res)
summary(pc.res)


plot(ggeffect(pc.res,terms=c("OMg.pergdw","root.d34S","graze")))

plot(ggeffect(pc.res,terms=c("root.d34S","graze")))
