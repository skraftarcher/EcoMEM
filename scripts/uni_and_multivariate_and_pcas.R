# analyze visual survey data----------------------------------------------------
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
  pivot_longer(-1:-5,names_to="taxa",values_to="pcover")%>%
  pivot_wider(names_from=quadrat,values_from=pcover,values_fill = 0)%>%
  pivot_longer(cols=starts_with("q"),names_to="quadrat",values_to="pcover")%>%
  group_by(date,blockID,plotID,sampling,taxa)%>%
  mutate(m.cover=mean(pcover))%>%
  pivot_wider(names_from=quadrat,values_from=pcover)%>%
  filter(taxa  == "T")

write.csv(pc2,"wdata/percent_cover.csv",row.names=F)

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

write.csv(can2,"wdata/canopy_height.csv",row.names=F)

#calculate shoots density data
shdn2<-shdn%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  select(date,sampling,blockID,plotID,taxa,quadrat,shoot.density)%>%
  mutate(taxa=ifelse(taxa=="T","Tt",taxa),
         shoot.density=as.numeric(shoot.density),
         shoot.density=ifelse(is.na(shoot.density),0,shoot.density),
         shoot.density=shoot.density/.01)%>%
  pivot_wider(names_from = taxa,values_from = shoot.density,values_fill = 0)%>%
  pivot_longer(Tt:H,names_to="taxa",values_to="sht.d")%>%
  filter(taxa  == "Tt")%>%
  group_by(date,sampling,blockID,plotID,taxa)%>%
  summarize(sht.d.m=round(mean(sht.d),2))%>%
  select(-taxa)

write.csv(shdn2,"wdata/shoot_density.csv",row.names=F)

#organize biodiversity dataset
bdiv2<-bdiv%>%
  select(-Surveyor,-Notes,-QAQC,-scientific.name,-common.name)%>%
  group_by(sampling,blockID,plotID,taxa)%>%
  summarize(total.n=sum(total.n))%>%
  pivot_wider(names_from = taxa,values_from = total.n,values_fill = 0)

#calculate visual survey species richness and diversity data
spr=specnumber(bdiv2[,-1:-3])
div=diversity(bdiv2[,-1:-3])

#join seagrass metric and bdiv dataframes
sg.bdiv<-bdiv2%>%
  left_join(pc2)%>%
  left_join(can2)%>%
  left_join(shdn2)%>%
  select(date,sampling,blockID,plotID,m.cover,plot.can,sht.d.m)

#identify graze/no graze plots
sg.bdiv <- sg.bdiv %>%
  mutate(grazing = case_when(
    str_ends(plotID, "G") ~ "graze",
    str_ends(plotID, "U") ~ "no graze"
  ))

#----linear regressions----

#percent cover
lmpcspr<-lm(spr~m.cover, data=sg.bdiv)
summary(lmpcspr)

lmpcdiv<-lm(div~m.cover, data=sg.bdiv)
summary(lmpcdiv)

#canopy height
lmcanspr<-lm(spr~plot.can, data=sg.bdiv)
summary(lmcanspr)

lmcandiv<-lm(div~plot.can, data=sg.bdiv)
summary(lmcandiv)

#shoot density
lmshdnspr<-lm(spr~sht.d.m, data=sg.bdiv)
summary(lmshdnspr)

lmshdndiv<-lm(div~sht.d.m, data=sg.bdiv)
summary(lmshdndiv)

#with grazing----

#percent cover
lmpcspr<-lm(spr~m.cover + grazing, data=sg.bdiv)
summary(lmpcspr)

lmpcdiv<-lm(div~m.cover + grazing, data=sg.bdiv)
summary(lmpcdiv)

#canopy height
lmcanspr<-lm(spr~plot.can + grazing, data=sg.bdiv)
summary(lmcanspr)

lmcandiv<-lm(div~plot.can + grazing, data=sg.bdiv)
summary(lmcandiv)

#shoot density
lmshdnspr<-lm(spr~sht.d.m + grazing, data=sg.bdiv)
summary(lmshdnspr)

lmshdndiv<-lm(div~sht.d.m + grazing, data=sg.bdiv)
summary(lmshdndiv)

#hellinger's transformation
bdiv2.hel<-decostand(bdiv2[-1:-3], "hellinger")
bdiv2.hel[is.na(bdiv2.hel)] <- 0

#pca
sg.bdiv2 <- sg.bdiv %>%
  select(m.cover,plot.can,sht.d.m)
bdiv2.rda<-rda(bdiv2.hel ~ ., data=sg.bdiv2[-1:-3], na.action = na.exclude)

plot(bdiv2.rda)
summary(bdiv2.rda)

#analyze dipnet data------------------------------------------------------------
# univariate analysis of species richness and diversity
# with respect to: % cover, shoot density, canopy height
# multivariate analysis using hellinger's transformation and pca analysis
# comparing individual taxa abundance among grazed/ungrazed plots

#get dipnet datasets and filter to just sampling two
#tax - taxa ids
dntax<-read_xlsx(paste0("odata/EXPERIMENT - LAB - Dipnet",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=4)
#bdiv - biodiversity
dnbdiv<-read_xlsx(paste0("odata/EXPERIMENT - LAB - Dipnet",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=2)%>%
  mutate(sample.date=case_when(
    sample.date >= samplings$Start.date[1] & sample.date <=samplings$End.date[1]~samplings$sampling[1],
    sample.date >= samplings$Start.date[2] & sample.date <=samplings$End.date[2]~samplings$sampling[2],
    sample.date >= samplings$Start.date[3] & sample.date <=samplings$End.date[3]~samplings$sampling[3],
    sample.date >= samplings$Start.date[4] & sample.date <=samplings$End.date[4]~samplings$sampling[4],
    sample.date >= samplings$Start.date[5] & sample.date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  filter(sample.date == "s2")

#calculate species richness and diversity
dnbdiv2<-dnbdiv%>%
  select(-date.processed,-lab.processor,-notes,-voucher,-tin.weight,-wet.weight,-dry.weight)%>%
  group_by(sample.date,blockID,plotID,taxaID)%>%
  summarize(abundance=sum(abundance))%>%
  pivot_wider(names_from = taxaID,values_from = abundance,values_fill = 0)

dnspr<-specnumber(dnbdiv2[,-1:-3])

dndiv<-diversity(dnbdiv2[,-1:-3])

#join seagrass metric and bdiv dataframes
sg.dn.bdiv<-dnbdiv2%>%
  left_join(pc2 %>% filter(sampling == "s2"))%>%
  left_join(can2 %>% filter(sampling == "s2"))%>%
  left_join(shdn2 %>% filter(sampling == "s2"))%>%
  select(date,sample.date,blockID,plotID,m.cover,plot.can,sht.d.m)

#identify graze/no graze plots
sg.dn.bdiv <- sg.dn.bdiv %>%
  mutate(grazing = case_when(
    str_ends(plotID, "G") ~ "graze",
    str_ends(plotID, "U") ~ "no graze"
  ))

#----linear regressions----

#percent cover
lmpcspr<-lm(dnspr~m.cover, data=sg.dn.bdiv)
summary(lmpcspr)

lmpcdiv<-lm(dndiv~m.cover, data=sg.dn.bdiv)
summary(lmpcdiv)

#canopy height
lmcanspr<-lm(dnspr~plot.can, data=sg.dn.bdiv)
summary(lmcanspr)

lmcandiv<-lm(dndiv~plot.can, data=sg.dn.bdiv)
summary(lmcandiv)

#shoot density
lmshdnspr<-lm(dnspr~sht.d.m, data=sg.dn.bdiv)
summary(lmshdnspr)

lmshdndiv<-lm(dndiv~sht.d.m, data=sg.dn.bdiv)
summary(lmshdndiv)

#with grazing----

#percent cover
lmpcspr<-lm(dnspr~m.cover + grazing, data=sg.dn.bdiv)
summary(lmpcspr)

lmpcdiv<-lm(dndiv~m.cover + grazing, data=sg.dn.bdiv)
summary(lmpcdiv)

#canopy height
lmcanspr<-lm(dnspr~plot.can + grazing, data=sg.dn.bdiv)
summary(lmcanspr)

lmcandiv<-lm(dndiv~plot.can + grazing, data=sg.dn.bdiv)
summary(lmcandiv)

#shoot density
lmshdnspr<-lm(dnspr~sht.d.m + grazing, data=sg.dn.bdiv)
summary(lmshdnspr)

lmshdndiv<-lm(dndiv~sht.d.m + grazing, data=sg.dn.bdiv)
summary(lmshdndiv)

#hellinger's transformation
dnbdiv2.hel<-decostand(dnbdiv2[-1:-3], "hellinger")
dnbdiv2.hel[is.na(dnbdiv2.hel)] <- 0

#pca
sg.dn.bdiv2 <- sg.dn.bdiv %>%
  select(m.cover,plot.can,sht.d.m)
dnbdiv2.rda<-rda(dnbdiv2.hel ~ ., data=sg.dn.bdiv2[-1:-3], na.action = na.exclude)

plot(dnbdiv2.rda)
summary(dnbdiv2.rda)
