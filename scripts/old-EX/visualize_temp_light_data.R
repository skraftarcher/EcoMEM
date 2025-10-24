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

dep<-read_xlsx("odata/downloaded_2024-06-18_EXPERIMENT - Plot Coordinates.xlsx",sheet=2)%>%
  select(Date,blockID,plotID)%>%
  distinct()%>%
  mutate(cutoff.date=ymd_hms(paste(Date,"18:00:00"),tz="America/New_York"))

graz<-read_xlsx("odata/downloaded_2024-06-18_EXPERIMENT - Grazing Simulation Data.xlsx",sheet=2)%>%
  select(gDate=Date,blockID)%>%
  distinct()

hobo<-hobo%>%
  mutate(date.time=ymd_hms(date.time),
         date.time=force_tz(date.time,tzone=ifelse(time.zone=="CDT","America/Chicago","America/New_York")))


hobo2<-hobo%>%
  left_join(dep)%>%
  filter(date.time>=cutoff.date)%>%
  select(-Date,-cutoff.date)%>%
  group_by(plotID)%>%
  mutate(max.date=max(date.time),
         bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"St. Andrews","St. Joe"))%>%
  filter(date.time<=max.date-minutes(20))%>%
  left_join(graz)%>%
  mutate(pre.treat=ifelse(date.time <= gDate,"pre","post"),
         treat=paste0(scar.treat,graze.treat))

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

# st joe bay by block

hobo2$pre.treat<-factor(hobo2$pre.treat,levels = c("pre","post"))
# temp
ggplot(hobo2%>%
         filter(bay=="St. Joe")%>%
         filter(pre.treat=="post")%>%
         filter(light.lux!=0))+
  geom_boxplot(aes(y=light.lux,fill=treat))+
  facet_wrap(~blockID,scales="free")+
  scale_color_viridis_d(option="D",begin=.4)

