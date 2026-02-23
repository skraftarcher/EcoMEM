# organize seagrass growth data
source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2

lp("tidyverse")
lp("readxl")

# load data
sg.bm<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=2,n_max=574)# remove n_max when FIU fixes datasheet
sg.s12<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=3)%>%
  filter(shoot.numb!="LL")%>%
  mutate(shoot.numb=as.numeric(shoot.numb))
sg.s3<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=4)%>%
  filter(shoot.numb!="LL")%>%
  mutate(shoot.numb=as.numeric(shoot.numb))
sg.s4<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=5)%>%
  filter(shoot.numb!="LL")%>%
  mutate(shoot.numb=as.numeric(shoot.numb))
sg.s5<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=6)%>%
  filter(shoot.numb!="LL")%>%
  mutate(shoot.numb=as.numeric(shoot.numb))
samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)
pinfo<-read.csv("odata/allplotinfo.csv")
sg.dates<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth.xlsx"),sheet=2)
  

# get all the growht data together

day.grow<-sg.dates%>%
  rename(date=Date.marked)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  select(plotID,sampling,days.of.growth)

sg.bm2<-sg.bm%>%
  rename(date=collect.date,sg.bm=sample.weight,plotID=siteID)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  filter(sample.type=="New Growth")%>%
  left_join(day.grow)%>%
  mutate(cg.bm=sg.bm/days.of.growth)%>%
  select(plotID,sampling,sg.bm)%>%
  group_by(plotID,sampling)%>%
  summarize(sg.bm=sum(sg.bm))


sg<-bind_rows(sg.s12,sg.s3,sg.s4,sg.s5)%>%
  mutate(areal.growth=new.length*width,
         areal.growth=ifelse(is.na(areal.growth),0,areal.growth))%>%
  group_by(siteID,collect.date,species,`processed by`,shoot.numb)%>%
  mutate(shoot.areal=sum(areal.growth),
         new.length=ifelse(leaf.numb==1 & new.length==0|is.na(new.length) & shoot.areal!=0,total.length,new.length),
         areal.growth=new.length*width,
         areal.growth=ifelse(is.na(areal.growth),0,areal.growth),
         shoot.areal=sum(areal.growth))%>%
  filter(shoot.areal!=0)%>%
  rename(plotID=siteID,date=collect.date)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  left_join(day.grow)%>%
  ungroup()%>%
  select(plotID,species,shoot.numb,sampling,days.of.growth,shoot.areal)%>%
  distinct()%>%
  mutate(areal.per.day=shoot.areal/days.of.growth)%>%
  group_by(plotID,sampling)%>%
  summarize(areal.grow.per.day.per.shoot=mean(areal.per.day),
            n.shoots=n())%>%
  left_join(samplings[,3:4])%>%
  left_join(pinfo)%>%
  select(sampling,mnths,blockID,plotID,bay,scar,graze,n.shoots,areal.grow.per.day.per.shoot)%>%
  left_join(sg.bm2)%>%
  mutate(g.sg.pershoot.perday=sg.bm/n.shoots)%>%
  select(-sg.bm)

write.csv(sg,"wdata/seagrass growth per day.csv",row.names = F)

# now get seagrass morphometrics
sg.morph<-bind_rows(sg.s12,sg.s3,sg.s4,sg.s5)%>%
  rename(plotID=siteID,date=collect.date)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  group_by(plotID,sampling)%>%
  summarize(max.n.leaves=max(leaf.numb),
            max.length=max(total.length,na.rm = T),
            max.width=max(width,na.rm = T),
            mean.width=mean(width,na.rm=T),
            mean.length=mean(total.length,na.rm = T))%>%
  left_join(samplings[,3:4])%>%
  left_join(pinfo)%>%
  select(sampling,mnths,blockID,plotID,bay,scar,graze,max.n.leaves,
         max.length,max.width,mean.width,mean.length)

write.csv(sg.morph,"wdata/seagrass morphometrics from growth.csv",row.names = F)
