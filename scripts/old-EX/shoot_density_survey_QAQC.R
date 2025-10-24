# QAQC shoot density data

#Stephanie K. Archer 06/16/2023

# load data----
source("scripts/download_shootdensity_survey-EX.R")

# load packages----
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
theme_set(theme_bw()+theme(panel.grid = element_blank()))

# Examine distribution of percent cover by taxa and surveyor----
shtd2<-shtd%>%
  mutate(date=mdy(date),
         mnth=month(date))%>%
  filter(taxa!="NA")

#visualize
ggplot(data=shtd2)+
  geom_boxplot(aes(x=shoot.density,fill=surveyor))+
  facet_grid(mnth~taxa,scales="free")

# ok if distributions largely overlap. If one or more observer starts to
# pull away from others then pause and go over methods as a group
# double check any extreme outliers
