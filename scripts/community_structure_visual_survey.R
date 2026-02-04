# analyze visual survey data
# univariate analysis of species richness and diversity
# with respect to: % cover, shoot density, canopy height
# multivariate analysis using hellinger's transformation and pca analysis

#install relevant datasheets
source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")

#load packages
lp("tidyverse")
lp("readxl")
lp("vegan")
library(obistools)

#----load data----
#pc - percent cover
pc<-read_xlsx(paste0("odata/EXPERIMENT - Seagrass and macroalgae Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=2)
#shdn - shoot density
shdn<-read_xlsx(paste0("odata/EXPERIMENT - Seagrass and macroalgae Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=3)
#can - canopy height
can<-read_xlsx(paste0("odata/EXPERIMENT - Seagrass and macroalgae Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=4)
#tax - taxa ids
tax<-read_xlsx(paste0("odata/EXPERIMENT - Visual Survey Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=4)
#samplings - sampling dates
samplings<-read_xlsx(paste0("odata/EXPERIMENT - Sampling Dates",format(Sys.time(), '_%d_%B_%Y'),".xlsx"))
#bdiv - biodiversity
bdiv<-read_xlsx(paste0("odata/EXPERIMENT - Visual Survey Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=2)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))

#calculate percent cover data
pc2<-pc%>%
  filter(quadrat<=4)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    taxa=case_when(
      taxa=="GRA"~"GRAC",
      taxa=="Grac"~"GRAC",
      taxa=="OB58"~"OB1",
      TRUE~taxa),
    quadrat=paste0("q",quadrat),
    percent.cover=(percent.cover/25)*100)%>%
  select(-Notes,-QAQC,-surveyor)%>%
  pivot_wider(names_from = taxa,values_from=percent.cover,values_fill=0)%>%
  pivot_longer(-1:-6,names_to="taxa",values_to="pcover")%>%
  pivot_wider(names_from=quadrat,values_from=pcover,values_fill = 0)%>%
  pivot_longer(cols=starts_with("q"),names_to="quadrat",values_to="pcover")%>%
  group_by(date,blockID,plotID,sampling,taxa)%>%
  mutate(m.cover=mean(pcover))%>%
  pivot_wider(names_from=quadrat,values_from=pcover)%>%
  filter(taxa %in% c("T", "S", "H"))%>%
  group_by(date,blockID,plotID,sampling) %>%
  summarize(cum.cover = sum(m.cover))

#calculate canopy height data
can2<-can%>%
  filter(quadrat<=4)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    quadrat=paste0("q",quadrat))%>%
  select(-notes,-grazing,-QAQC,-surveyor)%>%
  pivot_longer(cols=starts_with("c"),names_to="obs",values_to="can.height")%>%
  select(-obs)%>%
  group_by(date,blockID,plotID,sampling,quadrat)%>%
  summarize(m.can = mean(can.height, na.rm = TRUE), .groups = "drop")%>%
  pivot_wider(names_from = quadrat,values_from = m.can,values_fill = 0)%>%
  pivot_longer(cols=starts_with("q"),names_to="quadrat",values_to="can.height")%>%
  group_by(date,blockID,plotID,sampling)%>%
  summarize(plot.can = mean(can.height, na.rm = TRUE), .groups = "drop")

#calculate shoots density data
shdn2<-shdn%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  select(date,sampling,plotID,taxa,quadrat,shoot.density)%>%
  mutate(taxa=ifelse(taxa=="T","Tt",taxa),
         shoot.density=as.numeric(shoot.density),
         shoot.density=ifelse(is.na(shoot.density),0,shoot.density),
         shoot.density=shoot.density/.01)%>%
  pivot_wider(names_from = taxa,values_from = shoot.density,values_fill = 0)%>%
  pivot_longer(Tt:H,names_to="taxa",values_to="sht.d")%>%
  group_by(date,sampling,plotID,taxa)%>%
  summarize(sht.d.m=round(mean(sht.d),2))%>%
  pivot_wider(names_from = taxa,
              values_from = sht.d.m)%>%
  pivot_longer(H:Tt,names_to = "plot",values_to="sht.d.cum")%>%
  group_by(date,plotID,sampling) %>%
  summarize(cum.sht.d = sum(sht.d.cum))

#calculate species richness data
bdiv2<-bdiv%>%
  select(-Surveyor,-Notes,-QAQC,-scientific.name,-common.name)%>%
  group_by(sampling,blockID,plotID,taxa)%>%
  summarize(total.n=sum(total.n))%>%
  pivot_wider(names_from = taxa,values_from = total.n,values_fill = 0)

bdiv2$spr<-specnumber(bdiv2[,-1:-3])

#calculate species diversity
bdiv2$div<-diversity(bdiv2[,-1:-3])

#join seagrass metric and bdiv dataframes
sg.bdiv<-bdiv2%>%
  left_join(pc2)%>%
  left_join(can2)%>%
  left_join(shdn2)%>%
  select(date,sampling,blockID,plotID,spr,div,cum.cover,plot.can,cum.sht.d)

#----linear regressions----

#percent cover
lmpcspr<-lm(spr~cum.cover, data=sg.bdiv)
summary(lmpcspr)

lmpcdiv<-lm(div~cum.cover, data=sg.bdiv)
summary(lmpcdiv)

#canopy height
lmcanspr<-lm(spr~plot.can, data=sg.bdiv)
summary(lmcanspr)

lmcandiv<-lm(div~plot.can, data=sg.bdiv)
summary(lmcandiv)

#shoot density
lmshdnspr<-lm(spr~cum.sht.d, data=sg.bdiv)
summary(lmshdnspr)

lmshdndiv<-lm(div~cum.sht.d, data=sg.bdiv)
summary(lmshdndiv)

#hellinger's transformation

#pca

#directional analyses
