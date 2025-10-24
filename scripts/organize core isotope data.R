# organize seagrass core isotope data

library(tidyverse)
library(readxl)

# bring in data
skey<-read_xlsx("odata/sampleIDkey_core_analysis.xlsx")%>%
  select(sID="Isotope Lab Sample ID",
         plotID="PlotID",
         date="Date Sampled",
         component="Component")
r1<-read_xlsx("odata/250911 Report 4256 CN.xlsx",sheet="4256")%>%
  select(sID="Sample ID::Customer_Sample_ID",
         d15N="Sample ID::delta 15N",
         PN="Sample ID::Percent N",
         d13C="Sample ID::delta 13C",
         PC="Sample ID::Percent C")
r2<-read_xlsx("odata/250911 Report 4255.xlsx",sheet="4255")%>%
  select(sID="Sample ID::Customer_Sample_ID",
         d15N="Sample ID::delta 15N",
         PN="Sample ID::Percent N",
         d13C="Sample ID::delta 13C",
         PC="Sample ID::Percent C")

su<-read_xlsx("odata/250929 Report 4256 S.xlsx",sheet="4256")%>%
  select(sID="Sample ID::Customer_Sample_ID",
         d34S="Sample ID::delta 34S",
         PS="Sample ID::Percent S")

dat<-bind_rows(r1,r2)%>%
  left_join(skey)%>%
  mutate(CN=PC/PN,
         sampling=ifelse(date<"2025-01-01","pre","final"))


dats<-su%>%
  group_by(sID)%>%
  summarize(d34S=mean(d34S),
            PS=mean(PS))%>%
  left_join(skey)


dat<-left_join(dat,dats)

dat$scar<-"No scar"
dat$scar[grep(dat$plotID,pattern="SU")]<-"Scar"
dat$scar[grep(dat$plotID,pattern="SG")]<-"Scar"
dat$graze<-"No graze"
dat$graze[grep(dat$plotID,pattern="UG")]<-"Graze"
dat$graze[grep(dat$plotID,pattern="SG")]<-"Graze"
dat$bay<-"St. Joe"
dat$bay[grep(dat$plotID,pattern="SA")]<-"St. Andrew"

write.csv(dat,"wdata/core_isotopes.csv",row.names = FALSE)

