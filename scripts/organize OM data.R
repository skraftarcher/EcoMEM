# organize OM

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2

lp("tidyverse")
lp("readxl")
# load data
om<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sediment Organic Matter.xlsx"),sheet=2)
samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)
pinfo<-read.csv("odata/allplotinfo.csv")

# organize data
om2<-om%>%
  rename(date=date.collected)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    OM=(sed.dry.weight-sed.combust.weight)/(sed.dry.weight-tray.weight),
    dw.wt=sed.dry.weight/sed.wet.weight)%>%
  left_join(samplings[,3:4])%>%
  left_join(pinfo)%>%
  group_by(sampling,mnths,blockID,plotID,bay,scar,graze)%>%
  mutate(max.date=min(date.processed))%>%
  filter(date.processed==max.date)%>%
  select(sampling,mnths,blockID,plotID,bay,scar,graze,OMg.pergdw=OM,dw.wt)

write.csv(om2,"wdata/sediment OM data.csv",row.names = FALSE)
