# script to visualize seagrass data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")

pc<-read_xlsx("odata/downloaded_2024-06-21_EXPERIMENT - Seagrass and macroalgae Data.xlsx",sheet=2)%>%
  mutate(sampling=case_when(
    date < ymd("2024-05-20")~"s1",
    date > ymd("2024-05-20") & date < ymd("2024-07-01")~"s2"))


sd<-read_xlsx("odata/downloaded_2024-06-21_EXPERIMENT - Seagrass and macroalgae Data.xlsx",sheet=3)%>%
  mutate(m=month(date),
         y=year(date),
         sampling=paste0(m,"-",y))
can<-read_xlsx("odata/downloaded_2024-06-21_EXPERIMENT - Seagrass and macroalgae Data.xlsx",sheet=4)%>%
  mutate(m=month(date),
         y=year(date),
         sampling=paste0(m,"-",y))


# plot Thalassia percent cover

theme_set(theme_bw()+theme(panel.grid = element_blank()))

ggplot(pc%>%
         filter(taxa=="T")%>%
         mutate(pc=100*(as.numeric(percent.cover)/25))%>%
         separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
         mutate(grazed=ifelse(graze=="U","Not grazed","grazed")))+
  geom_boxplot(aes(x=pc,y=sampling,fill=sampling))+
  facet_grid(blockID~grazed,scales="free")


# organize sd data
sd2<-sd%>%
  filter(shoot.density!="NA")%>%
  mutate(shoot.density=as.numeric(shoot.density))%>%
  pivot_wider(names_from = taxa,values_from=shoot.density,values_fill=0)%>%
  select(-'NA')%>%
  pivot_longer('T':H,names_to="taxa",values_to = "shoot.density")%>%
  separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
  mutate(scar=ifelse(scar=="U","Not on scar","On Scar"),
         grazed=ifelse(graze=="U","Not grazed","grazed"))

ggplot(data=sd2%>%filter(taxa=="T"))+
  geom_boxplot(aes(y=blockID,x=shoot.density,fill=sampling))+
  facet_grid(scar~grazed)+
  ggtitle("Thalassia")

ggplot(data=sd2%>%filter(taxa=="S"))+
  geom_boxplot(aes(y=blockID,x=shoot.density,fill=sampling))+
  facet_grid(scar~grazed,scales="free")+
  ggtitle("Syringodium")

ggplot(data=sd2%>%filter(taxa=="H"))+
  geom_boxplot(aes(y=blockID,x=shoot.density,fill=sampling))+
  facet_grid(scar~grazed)+
  ggtitle("Halodule")



# look at time series with means and errors
sd3<-sd2%>%
  group_by(plotID,blockID,scar,grazed,sampling,taxa)%>%
  summarize(m.sd=mean(shoot.density),
            sd.sd=sd(shoot.density))

ggplot(sd3%>%
         filter(taxa=="T"))+
 # geom_errorbar(aes(x=sampling,ymin=m.sd-sd.sd,ymax=m.sd+sd.sd,color=grazed),position=position_dodge2(0.2))+
  geom_line(aes(x=sampling,y=m.sd,group=plotID,color=grazed,linetype = scar))+
  geom_point(aes(x=sampling,y=m.sd,group=plotID,color=grazed,size=sd.sd,pch=scar))+
  facet_wrap(~blockID)+
  scale_color_viridis_d(option="B",begin=.3,end=.7)

