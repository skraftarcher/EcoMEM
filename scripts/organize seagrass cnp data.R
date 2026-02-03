# script to organize nutrient contents of seagrass

# load datasets and packages
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("readxl")

samplings<-read_xlsx(paste0("odata/downloaded_2025-11-26_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)
pinfo<-read.csv("odata/allplotinfo.csv")

cnp1<-read_xlsx("odata/2025_05_30_CNPData_Plant_Whitman.xlsx")%>%
  rename(sid='Sample ID',
         PN='%N',
         PC='%C',
         PP='%P')%>%
  separate(sid,into = c("plotID","mnth","day","yr","e1"),sep="_")%>%
  select(-e1)%>%
  mutate(date=ymd(paste(yr,mnth,day)))%>%
  select(-yr,-mnth,-day)

cnp2<-read_xlsx("odata/2025_04_21_CNData_Plant_Whitman.xlsx")[,1:4]%>%
  rename(sid='Sample ID',
         PN='%N',
         PC='%C',
         PP='%P')%>%
  separate(sid,into = c("bay","plot","treat","mdyr","e1","e2"),sep="_")%>%
  mutate(bay=ifelse(bay=="SA","SA","SJ"))%>%
  select(-e1,-e2)%>%
  mutate(date=mdy(mdyr),
         plotID=paste0(bay,plot,treat))%>%
  select(-bay,-plot,-treat,-mdyr)


cnp3<-read_xlsx("odata/2025_06_25_CNPData_Plant_Whitman.xlsx")%>%
  rename(sid='Sample ID',
         PN='%N',
         PC='%C',
         PP='%P')%>%
  separate(sid,into = c("plotID","mnthday","yr","e1"),sep="_")%>%
  select(-e1)%>%
  mutate(date=ymd(paste(yr,mnthday)))%>%
  filter(!is.na(date))%>%
  select(-mnthday,-yr)

cnp<-full_join(pinfo,bind_rows(cnp1,cnp2,cnp3))%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))

write.csv(cnp,"wdata/cnp.csv",row.names = F)
