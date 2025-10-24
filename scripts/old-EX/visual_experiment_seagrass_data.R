# script to visualize seagrass data

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2
lp("tidyverse")
lp("readxl")
lp("lmerTest")
lp("glmmTMB")
lp("DHARMa")

samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)

pc<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass and macroalgae Data.xlsx"),sheet=2)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  left_join(samplings[,-1:-2])


sd<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass and macroalgae Data.xlsx"),sheet=3)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  left_join(samplings[,-1:-2])

can<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass and macroalgae Data.xlsx"),sheet=4)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  left_join(samplings[,-1:-2])


# plot Thalassia percent cover

theme_set(theme_bw()+theme(panel.grid = element_blank()))

ggplot(pc%>%
         filter(taxa=="T")%>%
         #filter(blockID %in% c("SJ1","SJ2","SJ3","SJ4","SJ5","SJ6"))%>%
         mutate(pc=100*(as.numeric(percent.cover)/25))%>%
         separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
         mutate(grazed=ifelse(graze=="U","Not grazed","grazed")))+
  geom_boxplot(aes(x=pc,y=sampling,fill=sampling))+
  facet_grid(blockID~grazed,scales="free")


# organize sd data
sd2<-sd%>%
  #filter(shoot.density!="NA")%>%
  mutate(shoot.density=as.numeric(shoot.density))%>%
  pivot_wider(names_from = taxa,values_from=shoot.density,values_fill=0)%>%
  pivot_longer('T':H,names_to="taxa",values_to = "shoot.density")%>%
  separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
  mutate(scar=ifelse(scar=="U","Not on scar","On Scar"),
         grazed=ifelse(graze=="U","Not grazed","grazed"))

#fixed #error - not uniquely identified rows - figure out what is duplicated
# sd.check<-sd%>%
#   group_by(date,surveyor,blockID,plotID,quadrat,sampling,taxa)%>%
#   summarize(n=n())
  

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

pc2<-pc%>%
  separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
  mutate(scar=ifelse(scar=="U","Not on scar","On Scar"),
         grazed=ifelse(graze=="U","Not grazed","grazed"),
         percent.cover=as.numeric(percent.cover))%>%
  group_by(plotID,blockID,scar,grazed,sampling,taxa)%>%
  summarize(m.pc=mean(percent.cover,na.rm=T),
            sd.pc=sd(percent.cover,na.rm=T))


ggplot(sd3%>%
         filter(taxa=="T"))+#%>%
         #filter(blockID %in% c("SJ1","SJ2","SJ3","SJ4","SJ5","SJ6")))+
 # geom_errorbar(aes(x=sampling,ymin=m.sd-sd.sd,ymax=m.sd+sd.sd,color=grazed),position=position_dodge2(0.2))+
  geom_line(aes(x=sampling,y=m.sd,group=plotID,color=grazed,linetype = scar))+
  geom_point(aes(x=sampling,y=m.sd,group=plotID,color=grazed,pch=scar))+
  facet_wrap(~blockID)+
  scale_color_viridis_d(option="B",begin=.3,end=.7)+
  ylab("Mean Thalassia Shoot Density")


ggplot(pc2%>%
         filter(taxa=="T"))+#%>%
         #filter(blockID %in% c("SJ1","SJ2","SJ3","SJ4","SJ5","SJ6")))+
  # geom_errorbar(aes(x=sampling,ymin=m.sd-sd.sd,ymax=m.sd+sd.sd,color=grazed),position=position_dodge2(0.2))+
  geom_line(aes(x=sampling,y=m.pc,group=plotID,color=grazed,linetype = scar))+
  geom_point(aes(x=sampling,y=m.pc,group=plotID,color=grazed,pch=scar))+
  facet_wrap(~blockID)+
  scale_color_viridis_d(option="B",begin=.3,end=.7)+
  ylab("Mean Thalassia Shoot Density")

# look at canopy height by date

can2<-can%>%
  filter(canopy1!="-")%>%
  separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
  mutate(scar=ifelse(scar=="U","Not on scar","On Scar"),
         grazed=ifelse(graze=="U","Not grazed","grazed"))%>%
  select(plotID,blockID,scar,grazed,sampling,canopy1,canopy2,canopy3,canopy4)%>%
  pivot_longer(canopy1:canopy4,names_to="n1",values_to="canopy")

can2<-can2%>%
  mutate(canopy=as.numeric(canopy),
         canopy=ifelse(is.na(canopy),0,canopy))%>%
  group_by(plotID,blockID,scar,grazed,sampling)%>%
  summarize(m.can=mean(canopy),
            sd.can=sd(canopy))

ggplot(can2)+#%>%
         #filter(blockID %in% c("SJ1","SJ2","SJ3","SJ4","SJ5","SJ6")))+
  # geom_errorbar(aes(x=sampling,ymin=m.sd-sd.sd,ymax=m.sd+sd.sd,color=grazed),position=position_dodge2(0.2))+
  geom_line(aes(x=sampling,y=m.can,group=plotID,color=grazed,linetype = scar))+
  geom_point(aes(x=sampling,y=m.can,group=plotID,color=grazed,pch=scar))+
  facet_wrap(~blockID)+
  scale_color_viridis_d(option="B",begin=.3,end=.7)+
  ylab("Mean Canopy Height (cm)")

# bring in sediment OM
om<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sediment Organic Matter.xlsx"),sheet=2)%>%
  mutate(sampling=case_when(
    date.collected >= samplings$Start.date[1] & date.collected <=samplings$End.date[1]~samplings$sampling[1],
    date.collected >= samplings$Start.date[2] & date.collected <=samplings$End.date[2]~samplings$sampling[2],
    date.collected >= samplings$Start.date[3] & date.collected <=samplings$End.date[3]~samplings$sampling[3],
    date.collected >= samplings$Start.date[4] & date.collected <=samplings$End.date[4]~samplings$sampling[4],
    date.collected >= samplings$Start.date[5] & date.collected <=samplings$End.date[5]~samplings$sampling[5]),
    OM=(sed.dry.weight-sed.combust.weight)/(sed.dry.weight-tray.weight))%>%
  separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
  mutate(scar=ifelse(scar=="U","Not on scar","On Scar"),
         grazed=ifelse(graze=="U","Not grazed","grazed"))

om.start<-om%>%
  filter(sampling=="s1")%>%
  select(blockID,plotID,start.OM=OM)

om.delta<-om%>%
  filter(sampling!="s1")%>%
  select(blockID,plotID,sampling,scar,graze,OM)%>%
  left_join(om.start)%>%
  mutate(delta.om=OM-start.OM)

ggplot(data=om)+
  geom_point(aes(x=blockID,y=OM,color=scar,shape=grazed))+
  facet_wrap(~sampling)

ggplot(om.delta)+
  geom_hline(aes(yintercept=0),linetype="dashed")+
  geom_point(aes(x=scar,y=delta.om,color=graze),size=3,alpha=.5)+
  facet_wrap(~blockID,scales="free")

can3<-can2%>%
  select(-sd.can)%>%
  pivot_wider(names_from = sampling,values_from=m.can)%>%
  mutate(delta.can=s5-s1,
         bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"SA","SJ"))%>%
  left_join(filter(om.delta,sampling=="s5"))

can4<-left_join(can2,om)%>%
  mutate(bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"SA","SJ"))

ggplot(can3)+
  geom_point(aes(x=OM,y=delta.can,color=grazed))+
  geom_smooth(aes(x=OM,y=delta.can,color=grazed),method="lm")+
  # geom_point(aes(x=delta.om,y=delta.can,color=grazed))+
  # geom_smooth(aes(x=delta.om,y=delta.can,color=grazed),method="lm")+
  ylab("Change in canopy height after 13 months")+
  xlab("g OM per g sediment (dry weight)")+
  scale_color_viridis_d(option="B",begin=.2,end=.8)

ggsave("figures/OM_can_13.jpg")

ggplot(can3)+
  geom_point(aes(x=start.OM,y=s1,color=scar))+
  geom_smooth(aes(x=start.OM,y=s1,color=scar),method="lm")+
  ylab("Canopy height at start of experiment")+
  xlab("g OM per g sediment (dry weight)")+
  scale_color_viridis_d(option="B",begin=.2,end=.8)+
  facet_wrap(~bay,scales="free")

ggplot(can4%>%
         filter(!is.na(graze)))+
  geom_point(aes(x=OM,y=m.can,color=graze,group=scar))+
  geom_smooth(aes(x=OM,y=m.can,color=graze,linetype=scar),method="lm")+
  ylab("Canopy height")+
  xlab("g OM per g sediment (dry weight)")+
  scale_color_viridis_d(option="B",begin=.2,end=.8)+
  facet_grid(sampling~bay,scales="free")

# look at change in shoot density over time
sd.s1<-filter(sd3,sampling=="s1")%>%
  ungroup()%>%
  select(-sampling,-sd.sd)%>%
  rename(start.sd=m.sd)

sd.delta<-filter(sd3,sampling!="s1")%>%
  left_join(sd.s1)%>%
  mutate(delta.sd=m.sd-start.sd,
         bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"St. Andrew","St Joe"))

ggplot(sd.delta%>%
         filter(taxa=="T"))+
  geom_hline(aes(yintercept = 0),linetype="dashed")+
  geom_boxplot(aes(x=scar,y=delta.sd,fill=grazed))+
  facet_grid(bay~sampling)


# look at change in pcover time
pc.s1<-filter(pc2,sampling=="s1")%>%
  ungroup()%>%
  select(-sampling,-sd.pc)%>%
  rename(start.pc=m.pc)

pc.delta<-filter(pc2,sampling!="s1")%>%
  left_join(pc.s1)%>%
  mutate(delta.pc=m.pc-start.pc,
         bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"St. Andrew","St Joe"))

ggplot(pc.delta%>%
         filter(taxa=="T"))+
  geom_hline(aes(yintercept = 0),linetype="dashed")+
  geom_boxplot(aes(x=scar,y=delta.pc,fill=grazed))+
  facet_grid(bay~sampling)

# look at change in canopy height over time
can.s1<-filter(can2,sampling=="s1")%>%
  ungroup()%>%
  select(-sampling,-sd.can)%>%
  rename(start.can=m.can)

can.delta<-filter(can2,sampling!="s1")%>%
  left_join(can.s1)%>%
  mutate(delta.can=m.can-start.can,
         bay=ifelse(blockID %in% c("SA1","SA2","SA3","SA4","SA5","SA6"),"St. Andrew","St Joe"))

ggplot(can.delta)+
  geom_hline(aes(yintercept = 0),linetype="dashed")+
  geom_boxplot(aes(x=scar,y=delta.can,fill=grazed))+
  facet_grid(bay~sampling)


# run some quick stats

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


sd<-sd%>%
  separate(plotID,into = c("bay","block","scar","grazed"),sep = c(2,3,4,5),remove = FALSE)%>%
  left_join(om)

sd$scar<-factor(sd$scar,levels = c("U","S"))
sd$grazed<-factor(sd$grazed,levels=c("U","G"))

sd2<-sd%>%
  filter(sampling!="s1")

sd.s<-sd%>%
  filter(sampling=="s1")%>%
  group_by(plotID,blockID)%>%
  summarize(start.shoot=mean(shoot.density))

sd2<-left_join(sd2,sd.s)

sd.lmer<-glmmTMB(shoot.density~scar*grazed*bay*+bay*grazed*mnths+scar*grazed*mnths+
                   (mnths|blockID)+offset(log(start.shoot))+(1|quadrat/plotID),
                 family=poisson(link=log),
                 data=sd2)


sd_simres <- simulateResiduals(sd.lmer)
testDispersion(sd_simres)
plot(sd_simres)

summary(sd.lmer)
lp("ggeffects")

graze.bay.sd<-ggeffect(sd.lmer,terms=c("mnths","grazed","bay"))

plot(graze.bay.sd)



