# porewater sulfide data visualization
library(tidyverse)

spp<-read.csv("wdata/porewater_sulfides_plots.csv")
spu<-read.csv("wdata/porewater_sulfides_urchinfronts.csv")


# plot urchin front each site as a line and going into the seagrass
ggplot(spu)+
  geom_line(aes(x=samp.loc,y=Concentration.um,group=SampleID))
# fairly consistently goes up in the seagrass

# look at change from grazed to ungrazed within a block by sampling

spp.2<-spp%>%
  group_by(bay,graze,blockID,sampling)%>%
  summarize(conc.um=mean(Concentration.um))

ggplot(spp.2)+
  geom_line(aes(x=graze,y=conc.um,group=sampling,color=sampling))+
  facet_wrap(~blockID,scales="free")
