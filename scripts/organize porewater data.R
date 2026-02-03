# organize porewater sulfide data

library(tidyverse)
library(readxl)


# read in data
sp<-read_xlsx("odata/sg_SA_h2s.xlsx",sheet = "forR")


# split into urchin front and plots

spp<-filter(sp,samp.loc=="center")%>%
  separate(SampleID,into = c("blockID","graze"),sep=-1,remove = FALSE)%>%
  mutate(scar="No.scar",
         graze=ifelse(graze=="G","Graze","No.graze"))

spp$bay<-"SJ"
spp$bay[grep(pattern="SA",spp$SampleID)]<-"SA"

spu<-filter(sp,samp.loc!="center")
spu$samp.loc<-factor(spu$samp.loc,levels=c("sand","edge","seagrass"))

write.csv(spp,"wdata/porewater_sulfides_plots.csv",row.names = F)
write.csv(spu,"wdata/porewater_sulfides_urchinfronts.csv",row.names = F)
