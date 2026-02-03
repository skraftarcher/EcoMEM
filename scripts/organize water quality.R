# organize water quality data

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2

lp("tidyverse")
lp("readxl")

fwq<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - water quality.xlsx"),sheet=2)
nh3<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - water quality lab.xlsx"),sheet=2)%>%
  rename(nh3.mgl=concentration)%>%
  select(-analyte,-notes)
nox<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - water quality lab.xlsx"),sheet=3)%>%
  rename(nox.ummol=concentration)%>%
  select(-analyte,-notes)
po4p<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - water quality lab.xlsx"),sheet=5)%>%
  rename(po4p.mgl=concentration)%>%
  select(-analyte,-notes)
samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)
# combine, clean up, identify missing data, then save
wq<-fwq%>%
  select(-surveyor,-tds,-Conduct,-notes,-QAQC,-sal)%>%
  left_join(nh3)%>%
  left_join(nox)%>%
  left_join(po4p)

needsnh3<-filter(wq,is.na(nh3.mgl))%>%
  mutate(needs.nh3=1)%>%
  select(blockID,date,needs.nh3)

needsnox<-filter(wq,is.na(nox.ummol))%>%
  mutate(needs.nox=1)%>%
  select(blockID,date,needs.nox)

needspo4p<-filter(wq,is.na(po4p.mgl))%>%
  mutate(needs.po4p=1)%>%
  select(blockID,date,needs.po4p)

needs<-full_join(needsnh3,needsnox)%>%
  full_join(needspo4p)


write.csv(needs,"wdata/experiment water needs.csv")

# turn bds into 0.0001
wq<-wq%>%
  mutate(nh3.mgl=as.numeric(ifelse(nh3.mgl=="bd","0.0001",nh3.mgl)),
         nox.ummol=as.numeric(ifelse(nox.ummol=="bd","0.0001",nox.ummol)),
         po4p.mgl=as.numeric(ifelse(po4p.mgl=="bd","0.0001",po4p.mgl)),
  sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  select(-date)

write.csv(wq,"wdata/water quality combined.csv",row.names = F)
