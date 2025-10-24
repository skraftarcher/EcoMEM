source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2

lp("tidyverse")
lp("readxl")
lp("lmerTest")

#load data
samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)

grow<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - seagrass growth.xlsx"),sheet=2)%>%
  mutate(sampling=case_when(
    Date.marked>= samplings$Start.date[1] & Date.marked<=samplings$End.date[1]~samplings$sampling[1],
    Date.marked>= samplings$Start.date[2] & Date.marked<=samplings$End.date[2]~samplings$sampling[2],
    Date.marked>= samplings$Start.date[3] & Date.marked<=samplings$End.date[3]~samplings$sampling[3],
    Date.marked>= samplings$Start.date[4] & Date.marked<=samplings$End.date[4]~samplings$sampling[4],
    Date.marked>= samplings$Start.date[5] & Date.marked<=samplings$End.date[5]~samplings$sampling[5]))%>%
  select(-notes)


growsummer2<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=3)%>%
  rename(Date.retrieved=collect.date,
         plotID=siteID)%>%
  left_join(grow)%>%
  filter(!is.na(new.length))%>%
  mutate(grow.area=new.length*width,
         leaf.area=total.length*width)%>%
  group_by(blockID,plotID, sampling,shoot.numb)%>%
  mutate(n.leaves=max(leaf.numb),
         max.length=max(total.length,na.rm = T),
         max.width=max(width,na.rm=T),
         tot.leaf.area=sum(leaf.area,na.rm = T),
         linear.grow=sum(new.length,na.rm=T),
         areal.grow=sum(grow.area,na.rm=T))

grownov2<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=4)%>%
  rename(Date.retrieved=collect.date,
         plotID=siteID)%>%
  left_join(grow)%>%
  filter(!is.na(new.length))%>%
  mutate(grow.area=new.length*width,
         leaf.area=total.length*width)%>%
  group_by(blockID,plotID, sampling,shoot.numb)%>%
  mutate(n.leaves=max(leaf.numb),
            max.length=max(total.length,na.rm = T),
            max.width=max(width,na.rm=T),
            tot.leaf.area=sum(leaf.area,na.rm = T),
            linear.grow=sum(new.length,na.rm=T),
            areal.grow=sum(grow.area,na.rm=T),
         shoot.numb=as.character(shoot.numb))


growmar<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=5)%>%
  rename(Date.retrieved=collect.date,
         plotID=siteID)%>%
  left_join(grow)%>%
  filter(!is.na(new.length))%>%
  mutate(grow.area=new.length*width,
         leaf.area=total.length*width)%>%
  group_by(blockID,plotID, sampling,shoot.numb)%>%
  mutate(n.leaves=max(leaf.numb),
         max.length=max(total.length,na.rm = T),
         max.width=max(width,na.rm=T),
         tot.leaf.area=sum(leaf.area,na.rm = T),
         linear.grow=sum(new.length,na.rm=T),
         areal.grow=sum(grow.area,na.rm=T),
         shoot.numb=as.character(shoot.numb))

growjul2<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass growth- Lab.xlsx"),sheet=6)%>%
  rename(Date.retrieved=collect.date,
         plotID=siteID)%>%
  left_join(grow)%>%
  filter(!is.na(new.length))%>%
  mutate(grow.area=new.length*width,
         leaf.area=total.length*width)%>%
  group_by(blockID,plotID, sampling,shoot.numb)%>%
  mutate(n.leaves=max(leaf.numb),
         max.length=max(total.length,na.rm = T),
         max.width=max(width,na.rm=T),
         tot.leaf.area=sum(leaf.area,na.rm = T),
         linear.grow=sum(new.length,na.rm=T),
         areal.grow=sum(grow.area,na.rm=T),
         shoot.numb=as.character(shoot.numb))


# now split into two - one to get morphometrics and the other to get growth excluding unmarked shoots
morph<-bind_rows(grownov2,growsummer2,growmar,growjul2)%>%
  ungroup()%>%
  group_by(blockID,plotID, sampling)%>%
  summarize(mn.n.leaves=mean(n.leaves),
            max.length=max(max.length,na.rm=T),
            mn.length=mean(total.length,na.rm=T),
            max.width=max(max.width,na.rm=T),
            mn.width=mean(width,na.rm=T),
            mn.leaf.area=mean(tot.leaf.area))

grow<-bind_rows(bind_rows(grownov2,growsummer2,growmar,growjul2))%>%
  filter(grow.area!=0)%>%
  ungroup()%>%
  mutate(linear.grow=linear.grow/days.of.growth,
         areal.grow=areal.grow/days.of.growth)%>%
  group_by(blockID,plotID, sampling)%>%
  summarize(mn.growpershoot.linear=mean(linear.grow,na.rm=T),
            mn.growpershoot.areal=mean(areal.grow,na.rm=T))

growmorph<-left_join(morph,grow)%>%
  separate(plotID,into = c("b2","scar","graze"),sep=c(3,4),remove=F)%>%
  select(-b2)%>%
  mutate(scar=ifelse(scar=="U","No Scar","Scarred"),
         graze=ifelse(graze=="U","Not grazed","Grazed"))
  
write.csv(growmorph,"wdata/seagrass growth and morph.csv",row.names=F)

# bring in loggers 

fls<-list.files("odata/data_loggers")
fls2<-fls[-grep(fls,pattern="ECOMEM")]
fls<-fls[grep(fls,pattern="ECOMEM")]

dl.org<-function(fn){
  t1<-read_xlsx(file.path("odata/data_loggers",fn))
  t2<-data.frame(fln=fn)%>%
    separate(fln,into=c("block","download.date",
                        "download.time","download.tz",
                        "ex1","data.tz"),sep=" ",convert = TRUE)%>%
    separate(data.tz,into=c("data.tz","ex2"),sep=-6)
  colnames(t1)[2]<-"date.time"
  
  if('Ch:1 - Temperature   (°C)' %in% colnames(t1)){
    t3<-t1%>%
      select(date.time,
             temp.c='Ch:1 - Temperature   (°C)',
             light.lux='Ch:2 - Light   (lux)')%>%
      mutate(date.time=force_tz(date.time,tzone=ifelse(t2$data.tz=="CDT","America/Chicago","America/New_York")),
             hoboID=t2$hoboID,
             time.zone=t2$data.tz)
  }
  if(!'Ch:1 - Temperature   (°C)' %in% colnames(t1)){
    t3<-t1%>%
      select(date.time,
             temp.c='Temperature (°C)',
             light.lux='Light (lux)')%>%
      mutate(date.time=force_tz(date.time,tzone=ifelse(t2$data.tz=="CDT","America/Chicago","America/New_York")),
             hoboID=t2$hoboID,
             time.zone=t2$data.tz)
  }
  t3$blockID<-t2$block
  return(t3)
}
log.plot<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - HoboLoggerBySite.xlsx"),sheet=2)

dl.org2<-function(fn){
  t1<-read_xlsx(file.path("odata/data_loggers",fn))
  t2<-data.frame(fln=fn)%>%
    separate(fln,into=c("ecomem","hoboID","download.date",
                        "download.time","download.tz",
                        "ex1","data.tz"),sep=" ",convert = TRUE)%>%
    separate(data.tz,into=c("data.tz","ex2"),sep=-6)%>%
    left_join(log.plot)%>%
    separate(plotID,into = c("block","ex"),sep = 3,remove=FALSE)
  colnames(t1)[2]<-"date.time"
  
  if('Ch:1 - Temperature   (°C)' %in% colnames(t1)){
    t3<-t1%>%
      select(date.time,
             temp.c='Ch:1 - Temperature   (°C)',
             light.lux='Ch:2 - Light   (lux)')%>%
      mutate(date.time=force_tz(date.time,tzone=ifelse(t2$data.tz=="CDT","America/Chicago","America/New_York")),
             hoboID=t2$hoboID,
             time.zone=t2$data.tz)
  }
  if(!'Ch:1 - Temperature   (°C)' %in% colnames(t1)){
    t3<-t1%>%
      select(date.time,
             temp.c='Temperature (°C)',
             light.lux='Light (lux)')%>%
      mutate(date.time=force_tz(date.time,tzone=ifelse(t2$data.tz=="CDT","America/Chicago","America/New_York")),
             hoboID=t2$hoboID,
             time.zone=t2$data.tz)
  }
  t3$blockID<-t2$block
  t3$plotID<-t2$plotID
  return(t3)
}


all<-fls2 %>%
  map_df(dl.org)%>%
  mutate(del=case_when(
    blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") & date.time >"2024-11-04 23:59:59"~1,
    !blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") & date.time >"2024-10-27 23:59:59"~1
  ),
  bay=case_when(
    blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") ~"SA",
    !blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") ~"SJ"))%>%
  filter(is.na(del))%>%
  select(-del)%>%
  group_by(blockID)%>%
  mutate(min.date=min(date.time))%>%
  ungroup()%>%
  group_by(bay)%>%
  mutate(min.date=max(min.date))%>%
  filter(date.time>min.date)

all2<-fls %>%
  map_df(dl.org2)%>%
  mutate(del=case_when(
    blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") & date.time >"2024-11-04 23:59:59"~1,
    !blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") & date.time >"2024-10-27 23:59:59"~1
  ),
  bay=case_when(
    blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") ~"SA",
    !blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6") ~"SJ"))%>%
  filter(is.na(del))%>%
  select(-del)%>%
  group_by(blockID)%>%
  mutate(min.date=min(date.time))%>%
  ungroup()%>%
  group_by(bay)%>%
  mutate(min.date=max(min.date))%>%
  filter(date.time>min.date)

all3<-bind_rows(all2,all)%>%
  mutate(sampling=case_when(
    date.time>= samplings$Start.date[1] & date.time<=samplings$End.date[1]~samplings$sampling[1],
    date.time>= samplings$Start.date[2] & date.time<=samplings$End.date[2]~samplings$sampling[2],
    date.time>= samplings$Start.date[3] & date.time<=samplings$End.date[3]~samplings$sampling[3],
    date.time>= samplings$Start.date[4] & date.time<=samplings$End.date[4]~samplings$sampling[4]))%>%
  filter(!is.na(sampling))
  

# visualize light
theme_set(theme_bw()+theme(panel.grid = element_blank()))
ggplot(data=all3,aes(x=date.time,y=light.lux,group=blockID,color=blockID))+
  geom_line()+
  facet_wrap(~sampling,scales="free_x")

# visualize temp
ggplot(data=all3,aes(x=date.time,y=temp.c,group=blockID,color=blockID))+
  geom_line()+
  facet_wrap(~sampling,scales="free")


lt<-all3%>%
  ungroup()%>%
  group_by(bay,blockID)%>%
  summarize(m.temp=mean(temp.c),
            max.temp=max(temp.c),
            sd.temp=sd(temp.c),
            m.light=mean(light.lux),
            max.light=max(light.lux),
            sd.light=sd(light.lux))

# bring in light then make some graphs
growmorph<-left_join(growmorph,lt)

growmorph.start<-filter(growmorph,sampling=="s1")%>%
  select(blockID,plotID,
         mn.n.leaves.start=mn.n.leaves,
         max.length.start=max.length,
         max.width.start=max.width,
         mn.leaf.area.start=mn.leaf.area,
         mn.growpershoot.linear.start=mn.growpershoot.linear,
         mn.growpershoot.areal.start=mn.growpershoot.areal)
growmorph<-filter(growmorph,sampling!="s1")%>%
  left_join(growmorph.start)%>%
  mutate(mn.n.leaves.delta=mn.n.leaves.start-mn.n.leaves,
         max.length.delta=max.length.start-max.length,
         max.width.delta=max.width.start-max.width,
         mn.leaf.area.delta=mn.leaf.area.start-mn.leaf.area,
         mn.growpershoot.linear.delta=mn.growpershoot.linear.start-mn.growpershoot.linear,
         mn.growpershoot.areal.delta=mn.growpershoot.areal.start-mn.growpershoot.areal)


(grow.s2<-ggplot(data=growmorph%>%
         filter(sampling=="s2"))+
  geom_hline(aes(yintercept=0),linetype="dashed")+
  geom_boxplot(aes(y=mn.growpershoot.areal.delta,fill=graze))+
  facet_grid(bay~scar,scales="free"))

(grow.s3<-ggplot(data=growmorph%>%
                   filter(sampling=="s3"))+
    geom_hline(aes(yintercept=0),linetype="dashed")+
    geom_boxplot(aes(y=mn.growpershoot.areal.delta,fill=graze))+
    facet_grid(bay~scar,scales="free"))
lp("patchwork")

grow.s2+grow.s3+plot_layout(guides="collect")

ggplot(data=growmorph%>%
         filter(!is.na(sampling)))+
  geom_point(aes(x=m.temp,y=mn.growpershoot.areal,color=graze),size=3)+
  geom_smooth(aes(x=m.temp,y=mn.growpershoot.areal,color=graze))+
  ylab("cm2 growth per shoot per day")+
  xlab("mean temperature during grow period")+
  scale_color_viridis_d(option="B",name="Grazing treatment",begin=.2,end=.8)

ggplot(data=growmorph%>%
         filter(!is.na(sampling)))+
  geom_point(aes(x=m.temp,y=mn.growpershoot.areal,color=scar),size=3)+
  geom_smooth(aes(x=m.temp,y=mn.growpershoot.areal,color=scar))+
  ylab("cm2 growth per shoot per day")+
  xlab("mean temperature during grow period")+
  scale_color_viridis_d(option="B",name="",begin=.2,end=.8)


growmorph$scar<-factor(growmorph$scar,levels=c("No Scar","Scarred"))
growmorph$graze<-factor(growmorph$graze,levels=c("Not grazed","Grazed"))

# linear model
lp("lmerTest")
par(mfrow=c(2,2))
grow.mod<-lmer(mn.growpershoot.areal~scar*sampling+graze*sampling +(1|blockID),data=growmorph)
plot(grow.mod)
summary(grow.mod)

  
# morphometrics
ggplot(data=growmorph)+
  geom_boxplot(aes(y=mn.n.leaves,fill=graze,x=scar))+
  ylab("mean # of leaves per shoot")+
  scale_color_viridis_d(option="B",name="Grazing treatment",begin=.2,end=.8)

ggplot(data=growmorph)+
  geom_boxplot(aes(y=max.length,fill=graze,x=scar))+
  ylab("maximum leaf length")+
  scale_color_viridis_d(option="B",name="Grazing treatment",begin=.2,end=.8)

ggplot(data=growmorph)+
  geom_boxplot(aes(y=mn.leaf.area,fill=graze,x=scar))+
  ylab("mean leaf area")+
  scale_color_viridis_d(option="B",name="Grazing treatment",begin=.2,end=.8)


# bring in OM and look at initial growth rates compared to OM

om<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sediment Organic Matter.xlsx"),sheet=2)%>%
  mutate(OM=(sed.dry.weight-sed.combust.weight)/sed.dry.weight,
         sampling=case_when(
    date.collected >= samplings$Start.date[1] & date.collected <=samplings$End.date[1]~samplings$sampling[1],
    date.collected >= samplings$Start.date[2] & date.collected <=samplings$End.date[2]~samplings$sampling[2],
    date.collected >= samplings$Start.date[3] & date.collected <=samplings$End.date[3]~samplings$sampling[3],
    date.collected >= samplings$Start.date[4] & date.collected <=samplings$End.date[4]~samplings$sampling[4]))%>%
  select(blockID,plotID,sampling,OM)%>%
  mutate(blockID=trimws(blockID),
         plotID=trimws(plotID))%>%
  separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
  mutate(scar=ifelse(scar=="U","Not on scar","On Scar"),
         grazed=ifelse(graze=="U","Not grazed","grazed"))

grow.s1<-filter(grow,sampling=="s1")%>%
  left_join(om)

ggplot(grow.s1)+
  geom_point(aes(x=OM,y=mn.growpershoot.areal,color=blockID))+
  facet_wrap(~scar,scales = "free")
