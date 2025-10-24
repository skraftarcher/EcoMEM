# QAQC canopy data

#Stephanie K. Archer 06/16/2023

# load data----
source("scripts/download_canopy_survey-EX.R")
canopy<-read.csv(paste0("odata/","canopy_survey",format(Sys.time(), '_%d_%B_%Y'),".csv"))

# load packages----
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
theme_set(theme_bw()+theme(panel.grid = element_blank()))

# Examine distribution of canopy heights by surveyor----
canopy2<-canopy%>%
  mutate(date=mdy(date),
         mnth=month(date))%>%
select(surveyor,mnth,siteID,quadrat,canopy1,canopy2,canopy3,canopy4)%>%
  pivot_longer(canopy1:canopy4,names_to = "canopy",values_to="canopy.ht")%>%
  select(-canopy)%>%
  filter(!is.na(canopy.ht))

#visualize
ggplot(data=canopy2)+
  geom_boxplot(aes(x=canopy.ht,fill=surveyor))+
  facet_wrap(~mnth,scales="free")

# ok if distributions largely overlap. If one or more observer starts to
# pull away from others then pause and go over methods as a group
