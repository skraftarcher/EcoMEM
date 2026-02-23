# organize data logger data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")

# bring in logger data
lt<-read.csv("wdata/hobodata_downloaded_2026-02-05.csv")%>%
  separate(date.time,into=c("date","time"),sep=" ")%>%
  mutate(time=ifelse(is.na(time),"00:00:00",time),
         date.time=ymd_hms(paste(date,time)),
         date.time=force_tz(date.time,tz=ifelse(time.zone=="CDT","America/Chicago","America/New_York")),
         date=ymd(date),
         plotID=ifelse(nchar(plotID)==5,plotID,paste0(blockID,plotID)))

ggplot(lt%>%
         filter(light.lux!=0))+
  geom_boxplot(aes(x=blockID,y=light.lux))

lt<-filter(lt,light.lux<20000)

samplings<-read_xlsx(paste0("odata/downloaded_2026-02-03_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)
date.established<-read_xlsx("odata/date_plot_established.xlsx",sheet=1)%>%
  rename(est=date)

lt<-lt%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    date.grazed=case_when(
      blockID %in% c("SJ1","SJ2","SJ3","SJ4")~ymd("2024-05-06"),
      blockID %in% c("SJ5","SJ6")~ymd("2024-05-05"),
      TRUE~ymd("2024-05-15")),
    prepost=ifelse(date < date.grazed,"pre","post"),
    bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"St. Andrews","St. Joe"))%>%
  left_join(date.established)%>%
  filter(date>est)

#save a light during sampling periods dataset

lt2<-lt%>%
  filter(!is.na(sampling))

write.csv(lt2,"wdata/raw_loggerdata_duringsamplings.csv",row.names = F)

# now make a summarized dataset during samplings
lt3<-lt2%>%
  left_join(samplings[,c(3,5,6)])%>%
  mutate(day=ifelse(hour(date.time)<sunrise & light.lux==0 | hour(date.time)>sunset & light.lux==0,"night","day"))%>%
  group_by(blockID,plotID,sampling,scar.treat,graze.treat,bay,prepost,day)%>%
  summarize(m.light=mean(light.lux,na.rm=T),
            se.light=sd(light.lux)/sqrt(n()-1))%>%
  filter(day=="day")%>%
  select(-day)

lt3b<-lt2%>%
  left_join(samplings[,c(3,5,6)])%>%
  group_by(blockID,plotID,sampling,scar.treat,graze.treat,bay,prepost)%>%
  summarize(m.temp=mean(temp.c,na.rm=T),
            se.temp=sd(temp.c)/sqrt(n()-1),
            min.temp=min(temp.c,na.rm=T),
            max.temp=max(temp.c,na.rm=T))%>%
  full_join(lt3)

write.csv(lt3b,"wdata/loggerdata summarized by plot and sampling.csv",row.names = F)

lt4<-lt2%>%
  left_join(samplings[,c(3,5,6)])%>%
  mutate(day=ifelse(hour(date.time)<sunrise & light.lux==0 | hour(date.time)>sunset & light.lux==0,"night","day"))%>%
  group_by(blockID,sampling,bay,prepost,day)%>%
  summarize(m.light=mean(light.lux,na.rm=T),
            se.light=sd(light.lux)/sqrt(n()-1))%>%
  filter(day=="day")%>%
  select(-day)

lt4b<-lt2%>%
  left_join(samplings[,c(3,5,6)])%>%
  group_by(blockID,sampling,bay,prepost)%>%
  summarize(m.temp=mean(temp.c,na.rm=T),
            se.temp=sd(temp.c)/sqrt(n()-1),
            min.temp=min(temp.c,na.rm=T),
            max.temp=max(temp.c,na.rm=T))%>%
  full_join(lt4)

write.csv(lt4b,"wdata/loggerdata summarized by block and sampling.csv",row.names = F)

# now organize a dataset that includes the pre-grazing and post grazing up until June 2024 sampling
lt5<-filter(lt,date<ymd("2024-06-09"))%>%
  left_join(samplings[,c(3,5,6)])%>%
  mutate(day=ifelse(hour(date.time)<sunrise & light.lux==0 | hour(date.time)>sunset & light.lux==0,"night","day"))%>%
  group_by(blockID,plotID,,scar.treat,graze.treat,bay,prepost,day)%>%
  summarize(m.light=mean(light.lux,na.rm=T),
            se.light=sd(light.lux)/sqrt(n()-1))%>%
  filter(day=="day")%>%
  select(-day)

lt5b<-filter(lt,date<ymd("2024-06-09"))%>%
  group_by(blockID,plotID,scar.treat,graze.treat,bay,prepost)%>%
  summarize(m.temp=mean(temp.c,na.rm=T),
            se.temp=sd(temp.c)/sqrt(n()-1),
            min.temp=min(temp.c,na.rm=T),
            max.temp=max(temp.c,na.rm=T))%>%
  full_join(lt5)

write.csv(lt5b,"wdata/loggerdata summarized by plotID and pre or post grazing between may and june.csv",row.names = F)

