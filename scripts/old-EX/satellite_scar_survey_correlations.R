# correlate data with scars from imagery

# load packages and data
source("scripts/install_packages_function.R")
source("scripts/download_labseagrass_biomass_survey-EX.R")
source("scripts/download_labseagrass_growth_survey-EX.R")
source("scripts/download_cnp_survey-EX.R")
source("scripts/download_imagery_survey-EX.R")
source("scripts/download_percentcover_survey-EX.R")
source("scripts/download_propeller_scar_survey-EX.R")
source("scripts/download_canopy_survey-EX.R")
source("scripts/download_shootdensity_survey-EX.R")
source("scripts/download_waterquality_survey-EX.R")
source("scripts/download_urchincount_survey-EX.R")
lp("tidyverse")
lp("lubridate")

# organize data

water<-water%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE)

water.oth<-filter(water,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))

water<-water%>%
  filter(!is.na(num))%>%
  bind_rows(water.oth)%>%
  mutate(bay=case_when(
    bay=="NB"~"North Bay",
    bay=="SAB"~"St. Andrew Bay",
    bay=="SJB"~"St. Joe Bay",
    bay=="WB"~"West Bay"))%>%
  select(bay,siteID)%>%
  distinct()


props.lengths<-propscar%>%
  mutate(g20.length=ifelse(is.na(as.numeric(length.shore))|is.na(as.numeric(length.water)),"yes","no"),
         tot.length=ifelse(g20.length=="no",
                           as.numeric(length.shore)+as.numeric(length.water),
                           20.1),
         length.cat=case_when(
           tot.length <=0 ~"0 m",
           tot.length >0 & tot.length<=5 ~ "1-5 m",
           tot.length > 5 & tot.length <=10~"6-10 m",
           tot.length > 10 & tot.length <=15~"11-15 m",
           tot.length > 15 & tot.length <=20~"16-20 m",
           tot.length >20 ~"+20 m"))%>%
  group_by(siteID,length.cat)%>%
  summarize(nscars=n())%>%
  pivot_wider(names_from = length.cat,names_prefix = "survey",values_from = nscars,values_fill=0)%>%
  mutate(scar.quantity.survey=`survey0 m`+`survey1-5 m`+`survey6-10 m`+`survey11-15 m`+`survey16-20 m`+`survey+20 m`)

scar_length<-scar_length%>%
  filter(!is.na(scarID))

scar_quant<-scar_quant%>%
  filter(!is.na(scar.quantity))%>%
  filter(scar.quantity!="")

biomass2<-biomass%>%
  filter(!is.na(tare.weight))%>%
  group_by(siteID,bay,sample.type)%>%
  summarize(sg.biomass=sum(sample.weight))%>%
  pivot_wider(names_from = sample.type,values_from=sg.biomass,values_fill=0)%>%
  mutate(sg.biomass=unmarked+newgrowth+oldgrowth)%>%
  mutate(bay=case_when(
    bay=="NB"~"North Bay",
    bay=="SAB"~"St. Andrew Bay",
    bay=="SJB"~"St. Joe Bay",
    bay=="WB"~"West Bay"))

bio3<-biomass%>%
  filter(!is.na(mark.date))%>%
  separate(mark.date,into=c("mark.m","mark.d","mark.y"),sep="\\.")%>%
  separate(collect.date,into=c("collect.m","collect.d","collect.y"),sep="\\.")%>%
  mutate(mark.date=ymd(paste(mark.y,mark.m,mark.d)),
         collect.date=ymd(paste(collect.y,collect.m,collect.d)),
         grow.days=as.numeric(collect.date-mark.date))%>%
  select(siteID,bay,grow.days)%>%
  distinct()

growth2<-growth%>%
  filter(!is.na(shoot.numb))%>%
  filter(!is.na(new.length))%>%
  left_join(bio3)%>%
  mutate(grow.area=total.length*width)%>%
  group_by(siteID,bay,species,shoot.numb)%>%
  summarize(tot.growth=sum(grow.area),
            grow.rate=tot.growth/grow.days)%>%
  distinct()%>%
  ungroup()%>%
  group_by(siteID,bay,species)%>%
  summarize(m.grow.rate=mean(grow.rate))%>%
  mutate(bay=case_when(
    bay=="NB"~"North Bay",
    bay=="SAB"~"St. Andrew Bay",
    bay=="SJB"~"St. Joe Bay",
    bay=="WB"~"West Bay"))


growth3<-growth%>%
  filter(!is.na(shoot.numb))%>%
  filter(!is.na(new.length))%>%
  group_by(siteID,bay,species)%>%
  summarize(m.length=mean(total.length),
            max.length=max(total.length),
            m.width=mean(width),
            max.width=max(width))%>%
  mutate(bay=case_when(
    bay=="NB"~"North Bay",
    bay=="SAB"~"St. Andrew Bay",
    bay=="SJB"~"St. Joe Bay",
    bay=="WB"~"West Bay"))

canopy2<-canopy%>%
  pivot_longer(canopy1:canopy4,names_to="can",values_to="hts")%>%
  group_by(date,siteID)%>%
  summarize(m.ht=mean(hts,na.rm = TRUE),
            max.ht=max(hts,na.rm = TRUE))%>%
  mutate(date=mdy(date))%>%
  ungroup()%>%
  group_by(siteID)%>%
  filter(date==max(date))

pc2<-pc%>%
  mutate(percent.cover=percent.cover/25,
         taxa=trimws(taxa))%>%
  pivot_wider(names_from=taxa,values_from = percent.cover,values_fill=0)%>%
  pivot_longer(`T`:ACE,names_to = "taxa",values_to="percent.cover")%>%
  group_by(date,siteID,taxa)%>%
  summarize(m.pc=mean(percent.cover,na.rm = T))%>%
  mutate(date=mdy(date))%>%
  filter(taxa %in% c("T","H","He","S"))%>%
  pivot_wider(names_from = taxa,names_prefix = "pc.",values_from = m.pc)%>%
  ungroup()%>%
  group_by(siteID)%>%
  filter(date==max(date))%>%
  select(-date)

shtd2<-shtd%>%
  mutate(taxa=trimws(taxa))%>%
  filter(!is.na(quadrat))%>%
  pivot_wider(names_from=taxa,values_from = shoot.density,values_fill=0)%>%
  pivot_longer(`T`:He,names_to = "taxa",values_to="shoot.density")%>%
  group_by(date,siteID,taxa)%>%
  summarize(m.shtd=mean(shoot.density,na.rm = T))%>%
  mutate(date=mdy(date))%>%
  pivot_wider(names_from = taxa,names_prefix = "shtd.",values_from = m.shtd)%>%
  ungroup()%>%
  group_by(siteID)%>%
  filter(date==max(date))%>%
  select(-date)

sat.length<-scar_length%>%
  group_by(siteID)%>%
  mutate(total.scar.length=sum(scar.length.clipped),
         length.cat=case_when(
           scar.length.clipped <=0 ~"0 m",
           scar.length.clipped >0 & scar.length.clipped<=5 ~ "1-5 m",
           scar.length.clipped > 5 & scar.length.clipped <=10~"6-10 m",
           scar.length.clipped > 10 & scar.length.clipped <=15~"11-15 m",
           scar.length.clipped > 15 & scar.length.clipped <=20~"16-20 m",
           scar.length.clipped >20 ~"+20 m"))%>%
  filter(!is.na(total.scar.length))%>%
  group_by(siteID,length.cat,total.scar.length)%>%
  summarize(nscars=n())%>%
  pivot_wider(names_from = length.cat,names_prefix = "sat",values_from = nscars,values_fill=0)

sat.quant=scar_quant%>%
  filter(!is.na(scar.quantity))

sat.prop<-left_join(sat.quant,sat.length)%>%
  select(-notes)%>%
  mutate(across(total.scar.length:`sat6-10 m`,~ifelse(is.na(.x),0,.x)))
  
sg_cnp.um<-sg_cnp%>%
  rename(PN.um=PN,
         PC.um=PC,
         PP.um=PP)%>%
  filter(sample.type=="UM")%>%
  select(-sample.type)%>%
  left_join(sg_IDs)%>%
  select(-sampleID)

sg_cnp.og<-sg_cnp%>%
  rename(PN.og=PN,
         PC.og=PC,
         PP.og=PP)%>%
  filter(sample.type=="OG")%>%
  select(-sample.type)%>%
  left_join(sg_IDs)%>%
  select(-sampleID)

sg_cnp.ng<-sg_cnp%>%
  rename(PN.ng=PN,
         PC.ng=PC,
         PP.ng=PP)%>%
  filter(sample.type=="NG")%>%
  select(-sample.type)%>%
  left_join(sg_IDs)%>%
  select(-sampleID)

sg_cnp2<-left_join(sg_cnp.um,sg_cnp.ng)%>%
  left_join(sg_cnp.og)%>%
  filter(!is.na(siteID))

full.dat1<-left_join(water,sat.prop)%>%
  filter(!is.na(total.scar.length))%>%
  left_join(shtd2)%>%
  left_join(pc2)%>%
  left_join(canopy2)%>%
  filter(!is.na(pc.T))%>%
  left_join(props.lengths)%>%
  mutate(scar.dens.sat=scar.quantity/31415.93,
         scar.dens.surv=scar.quantity.survey/50)

full.dat2<-left_join(water,sat.prop)%>%
  left_join(growth2)%>%
  left_join(growth3)%>%
  left_join(canopy2)%>%
  left_join(sg_cnp2)%>%
  left_join(biomass2)%>%
  filter(!is.na(unmarked))%>%
  filter(!is.na(scar.quantity))%>%
  left_join(props.lengths)%>%
  mutate(scar.dens.sat=scar.quantity/31415.93,
         scar.dens.surv=scar.quantity.survey/50)%>%
  left_join(full.dat1)

write.csv(full.dat2,"wdata/scars_growth_combined.csv",row.names = FALSE)


# look at correlation between scar density from surveys and satellite
theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           axis.text = element_text(size=12),
                           axis.title = element_text(size=14))) 

grow.lm<-lm(log10(m.grow.rate)~scar.dens.sat,data=full.dat2)

par(mfrow=c(2,2))
plot(grow.lm)
summary(grow.lm)


shtdt.lm<-lm((shtd.H)/.01~scar.dens.sat*bay,data=full.dat1)
plot(shtdt.lm)
summary(shtdt.lm)

biom.lm<-lm(log10(sg.biomass/0.017671)~scar.dens.sat,data=full.dat2)
plot(biom.lm)
summary(biom.lm)

grow.lab<-expression(paste("log"[10],italic(" T. testudinum")," growth (mm"^2,"day"^"-1",")"))
shoot.lab<-expression(paste(italic(" T. testudinum"), " shoots m"^2))
scar.dens.lab<-expression(paste("Propeller Scars m"^2))
biomass.lab<-expression(paste("log"[10]," Seagrass aboveground biomass g m "^"-2"))
growr2<-expression(paste("R"^2," = 0.61"))
shootr2<-expression(paste("R"^2," = 0.24"))
biomr2<-expression(paste("R"^2," = 0.27"))


ggplot(data=full.dat2,aes(x=scar.dens.sat,y=log(m.grow.rate)))+
  geom_point(size=3)+
  geom_smooth(method="lm",color="black")+
  ylab(grow.lab)+
  xlab(scar.dens.lab)+
  annotate("text",label=growr2,x=0.002,y=6.2,size=10)
ggsave("figures/scar density_grow rate.jpg",dpi=400,width=7,height = 5)


ggplot(data=full.dat1,aes(x=scar.dens.sat,y=shtd.T/.01))+
  geom_point(size=3)+
  geom_smooth(method="lm",color="black")+
  ylab(shoot.lab)+
  xlab(scar.dens.lab)+
  annotate("text",label=shootr2,x=0.0005,y=2500,size=10)

ggsave("figures/scar density_shoot density.jpg",dpi=400,width=7,height = 5)


ggplot(data=full.dat2,aes(x=scar.dens.sat,y=log10(sg.biomass/0.017671)))+
  geom_point(size=3)+
  geom_smooth(method="lm",color="black")+
  ylab(biomass.lab)+
  xlab(scar.dens.lab)+
  annotate("text",label=biomr2,x=0.0018,y=2.25,size=10)

ggsave("figures/scar density_sg_biomass.jpg",dpi=400,width=7,height = 5)

# scaqr categories figure
sat.length2<-scar_length%>%
  group_by(siteID)%>%
  mutate(total.scar.length=sum(scar.length.clipped),
         length.cat=case_when(
           scar.length.clipped <=0 ~"0 m",
           scar.length.clipped >0 & scar.length.clipped<=5 ~ "1-5 m",
           scar.length.clipped > 5 & scar.length.clipped <=10~"6-10 m",
           scar.length.clipped > 10 & scar.length.clipped <=15~"11-15 m",
           scar.length.clipped > 15 & scar.length.clipped <=20~"16-20 m",
           scar.length.clipped >20 ~"+20 m"))%>%
  filter(!is.na(total.scar.length))%>%
  left_join(water)%>%
  group_by(bay,length.cat)%>%
  summarize(nscars=n())


sat.length2$length.cat<-factor(sat.length2$length.cat,levels=c("1-5 m","6-10 m","11-15 m","16-20 m","+20 m"))
sat.length2$bay<-factor(sat.length2$bay,levels=c("North Bay","West Bay","St. Andrew Bay","St. Joe Bay"))

bay.colors<-data.frame(bay=c("North Bay","West Bay","St. Andrew Bay","St. Joe Bay"),
                       colrs=c("#99d8c9","#41ae76","#005824","#d8b365"))


ggplot(data=sat.length2)+
  geom_bar(aes(x=length.cat,y=nscars,fill=bay),position=position_dodge(),stat = "identity",color="black")+
  xlab("Length of propeller scars")+
  ylab("Number of scars")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=14))+
  scale_fill_manual(values=bay.colors[,2],name="")

ggsave("figures/propeller_scars_by_bay_satellite.jpg",dpi=400,height=5,width=7)


# make a CNP figure
cnp<-sg_cnp%>%
  rename(seagrass.part=sample.type)%>%
  left_join(sg_IDs)%>%
  filter(sample.type=="sg")%>%
  mutate(PP=ifelse(PP=="nes",NA,as.numeric(PP)))%>%
  pivot_longer(PN:PP,names_to="nutrient",values_to="percent")%>%
  group_by(bay,nutrient)%>%
  summarize(n.samps=n(),
            m.p=mean(percent,na.rm=T),
            se.pn=sd(percent)/sqrt(n.samps))%>%
  mutate(nutrient=case_when(
    nutrient=="PP"~"Phosphorus",
    nutrient=="PC"~"Carbon",
    nutrient=="PN"~"Nitrogen"))

cnp$bay<-factor(cnp$bay,levels=c("North Bay","West Bay","St. Andrew Bay","St. Joseph Bay"),
                labels=c("North Bay","West Bay","St. Andrew Bay","St. Joe Bay"))


ggplot(data=cnp,aes(x=nutrient,fill=bay))+
  geom_errorbar(aes(ymin=m.p-se.pn,ymax=m.p+se.pn),
                position = position_dodge(1),
                linewidth=.1)+
  geom_point(aes(y=m.p,color=bay),position = position_dodge(1),size=5)+
  facet_wrap(~nutrient,scales="free")+
  ylab("Percent of seagrass tissue")+
  xlab("")+
  scale_color_manual(values=bay.colors[,2])+
  theme(axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        strip.text = element_text(size=14),
        legend.position = "top")

ggsave("figures/seagrass_nutrients.jpg",dpi=400,width=10,height=5)

# see if there's a link between prop scars and urchins

urch<-urchin_count%>%
  group_by(siteID)%>%
  summarise(urchins=mean(tally))

full.dat3<-left_join(full.dat1,urch)%>%
  filter(bay %in% c("St. Joe Bay","St. Andrew Bay"))

plot(full.dat3$urchins~full.dat3$scar.dens.sat)


# no relationship

#relationship between urchins and nutrient content?
cnp.urch<-sg_cnp%>%
  rename(seagrass.part=sample.type)%>%
  left_join(sg_IDs)%>%
  filter(sample.type=="sg")%>%
  mutate(PP=ifelse(PP=="nes",NA,as.numeric(PP)))%>%
  pivot_longer(PN:PP,names_to="nutrient",values_to="percent")%>%
  left_join(urch)

ggplot(cnp.urch,aes(x=percent,y=urchins))+
  geom_point(aes(color=bay))+
  facet_wrap(bay~nutrient,scales="free")

#nope

# make an unrchin plot
urch.bay<-urch%>%
  left_join(water)%>%
  group_by(bay)%>%
  summarize(n.sites=n(),
    m.urch=mean(urchins/50),
    se.urch=sd(urchins/50)/sqrt(n.sites))
urch.bay$bay<-factor(urch.bay$bay,levels=c("North Bay","West Bay","St. Andrew Bay","St. Joe Bay"))

urchlab<-expression(paste("Urchins m"^"-2"))
ggplot(data=urch.bay)+
  geom_errorbar(aes(x=1,ymin=m.urch-se.urch,ymax=m.urch+se.urch,group=bay),
                position=position_dodge(1))+
  geom_point(aes(x=1,y=m.urch,color=bay),position=position_dodge(1),size=7)+
  scale_color_manual(values=bay.colors[,2])+
  theme(legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())+
  ylab(urchlab)

ggsave("figures/urchindensity.jpg",dpi=400,width=7,height=4)

# scar density comparisons
scar.dens<-full.dat1%>%
  select(bay,siteID,scar.dens.sat,scar.dens.surv)%>%
  pivot_longer(scar.dens.sat:scar.dens.surv,names_to = "stype",values_to = "sdens")%>%
  mutate(stype=ifelse(stype=="scar.dens.surv","Survey","Satellite"))%>%
  group_by(bay,stype)%>%
  summarize(n.sites=n(),
    m.dens=mean(sdens),
    se.dens=sd(sdens)/sqrt(n.sites))

scar.dens$bay<-factor(scar.dens$bay,levels=c("North Bay","West Bay","St. Andrew Bay","St. Joe Bay"))


ggplot(data=scar.dens,aes(x=bay,shape=stype,color=bay))+
  geom_errorbar(aes(ymin=m.dens-se.dens,ymax=m.dens+se.dens),position=position_dodge(0.5),linewidth=.1)+
  geom_point(aes(y=m.dens),position = position_dodge(.5),size=5)+
  ylab(scar.dens.lab)+
  xlab("")+
  scale_color_manual(values=bay.colors[,2],name="")+
  scale_shape_manual(name="Data source",values=c(15,16))

ggsave("figures/prop scar density compare methods.jpg",dpi=400,width=7,height=4)
