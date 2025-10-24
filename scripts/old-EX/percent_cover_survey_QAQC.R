# QAQC percent cover data

#Stephanie K. Archer 06/16/2023

# load data----
source("scripts/download_percentcover_survey-EX.R")
pc<-read.csv(paste0("odata/","percent_cover_survey",format(Sys.time(), '_%d_%B_%Y'),".csv"))

# load packages----
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
theme_set(theme_bw()+theme(panel.grid = element_blank()))

# Examine distribution of percent cover by taxa and surveyor----
pc2<-pc%>%
  mutate(date=mdy(date),
         mnth=month(date))%>%
  filter(taxa!="NA")

#visualize
ggplot(data=pc2)+
  geom_boxplot(aes(x=percent.cover,fill=surveyor))+
  facet_grid(taxa~mnth,scales="free")

# ok if distributions largely overlap. If one or more observer starts to
# pull away from others then pause and go over methods as a group
