# script to visualize logger data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")

# bring in logger data
fls<-list.files("wdata",pattern = "hobodata")

for(i in 1:length(fls)){
  t1<-read.csv(file.path("wdata",fls[i]))
  if(i==1)hobo<-t1
  if(i!=1)hobo<-rbind(hobo,t1)
}

# bring in plot delineation dataset and crop off pre-deployment data

dep<-read_xlsx("odata/downloaded_2024-05-14_EXPERIMENT - Plot Coordinates.xlsx",sheet=2)%>%
  select(Date,blockID,plotID)%>%
  distinct()%>%
  mutate(cutoff.date=ymd_hms(paste(Date,"18:00:00"),tz="America/New_York"))

hobo1<-filter(hobo,time.zone=="America/New_York")%>%
  mutate(date.time=ymd_hms(date.time,tz="America/New_York"))
hobo2<-filter(hobo,time.zone!="America/New_York")%>%
  mutate(date.time=ymd_hms(date.time,tz="America/Chicago"))


hobo2<-bind_rows(hobo1,hobo2)%>%
  left_join(dep)%>%
  filter(date.time>=cutoff.date)%>%
  select(-Date,-cutoff.date)%>%
  group_by(plotID)%>%
  mutate(max.date=max(date.time),
         bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"St. Andrews","St. Joe"))%>%
  filter(date.time<=max.date-minutes(20))

# now plot!

theme_set(theme_bw()+theme(panel.grid = element_blank()))

#temp
ggplot(hobo2)+
  geom_line(aes(x=date.time,y=temp.c,group=plotID,color=blockID))+
  facet_wrap(vars(bay),nrow=2,scales="free_x")+
  scale_color_viridis_d(option="D")

#light
ggplot(hobo2)+
  geom_line(aes(x=date.time,y=light.lux,group=plotID,color=blockID))+
  facet_wrap(vars(bay),nrow=2,scales="free_x")+
  scale_color_viridis_d(option="D")


