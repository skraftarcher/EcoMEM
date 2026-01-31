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

#join percent cover and bdiv dataframes
bdiv2<-bdiv%>%
  left_join(pc)%>%
  select(date,blockID,plotID,taxa,percent.cover)

#calculate species richness

#calculate species diversity

#linear regression

#hellinger's transformation

#pca

#directional analyses
