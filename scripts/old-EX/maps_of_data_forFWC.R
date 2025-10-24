# make maps to visualize survey data

# Stephanie K. Archer 

# load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("ggspatial")
lp("sf")
#lp("rgdal")
lp("sp")
lp("vegan")
lp("lubridate")
lp("patchwork")
lp("readxl")
lp("raster")
source("scripts/download_propscar_experiment-EX.R")
# load data
exp.coords<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_Experiment - Plot Coordinates.xlsx"),sheet = 2)
surv.coords<-read_xlsx("odata/SURVEY - water quality.xlsx",sheet=2)
exp.pc<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_Experiment - Seagrass and macroalgae Data.xlsx"),sheet = 2)
exp.sd<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_Experiment - Seagrass and macroalgae Data.xlsx"),sheet = 3)
surv.pc<-read_xlsx("odata/SURVEY - Macrophyte Community.xlsx",sheet=2)
surv.sd<-read_xlsx("odata/SURVEY - Macrophyte Community.xlsx",sheet=4)


# organize pc and sd data

exp.pc2<-exp.pc%>%
  dplyr::select(-Notes,-QAQC)%>%
  rename("sample.date"=date)%>%
  mutate(percent.cover=(as.numeric(percent.cover)/25)*100)%>%
  mutate(sampling=case_when(
    sample.date >= samplings$Start.date[1] & sample.date <=samplings$End.date[1]~samplings$sampling[1],
    sample.date >= samplings$Start.date[2] & sample.date <=samplings$End.date[2]~samplings$sampling[2],
    sample.date >= samplings$Start.date[3] & sample.date <=samplings$End.date[3]~samplings$sampling[3]))%>%
  group_by(sampling,blockID,taxa)%>%
  summarize(pc=round(mean(percent.cover,na.rm = T),2))%>%
  pivot_wider(names_from = taxa,values_from=pc,values_fill = 0)%>%
  dplyr::select(sampling,blockID,"Tt_pc"='T',"Hw_pc"=H,"Sf_pc"=S)%>%
  filter(!is.na(sampling))

exp.sd2<-exp.sd%>%
  dplyr::select(-notes,-QAQC)%>%
  rename("sample.date"=date)%>%
  mutate(shoot.density=(as.numeric(shoot.density)/.01))%>%
  mutate(sampling=case_when(
    sample.date >= samplings$Start.date[1] & sample.date <=samplings$End.date[1]~samplings$sampling[1],
    sample.date >= samplings$Start.date[2] & sample.date <=samplings$End.date[2]~samplings$sampling[2],
    sample.date >= samplings$Start.date[3] & sample.date <=samplings$End.date[3]~samplings$sampling[3]))%>%
  group_by(sampling,blockID,taxa)%>%
  summarize(sd=round(mean(shoot.density,na.rm = T),0))%>%
  pivot_wider(names_from = taxa,values_from=sd,values_fill = 0)%>%
  dplyr::select(sampling,blockID,"Tt_sd"='T',"Hw_sd"=H,"Sf_sd"=S)


surv.pc2<-surv.pc%>%
  dplyr::select(-Notes,-QAQC)%>%
  mutate(percent.cover=(as.numeric(percent.cover)/25)*100)%>%
  group_by(date,siteID,taxa)%>%
  summarize(pc=round(mean(percent.cover,na.rm = T),2))%>%
  pivot_wider(names_from = taxa,values_from=pc,values_fill = 0)%>%
  dplyr::select(date,siteID,"Tt_pc"='T',
         "Hw_pc"=H,
         "Sf_pc"=S,
         "He_pc"=He)
  

surv.sd2<-surv.sd%>%
  dplyr::select(-notes,-QAQC)%>%
  mutate(shoot.density=(as.numeric(shoot.density)/.01))%>%
  group_by(date,siteID,taxa)%>%
  summarize(sd=round(mean(shoot.density,na.rm = T),0))%>%
  pivot_wider(names_from = taxa,values_from=sd,values_fill = 0)%>%
  dplyr::select(date,siteID,"Tt_sd"='T',
         "Hw_sd"=H,
         "Sf_sd"=S,
         "He_sd"=He)

# combine data to make a map for the survey

surv.map<-surv.coords%>%
  dplyr::select(siteID,lat,long,date)%>%
  distinct()%>%
  left_join(surv.pc2)%>%
  left_join(surv.sd2)%>%
  filter(!is.na(Tt_pc))%>%
  mutate(siteID=paste0("ECOMEM.",row.names(.)),
         long=long*-1)%>%
  rename("SurvDate"=date)

surv.shape<-surv.map
coordinates(surv.shape)=~long+lat
proj4string(surv.shape)=CRS("+proj=longlat +datum=WGS84")

raster::shapefile(surv.shape, "shapefiles/ECOMEM_Survey_Shapefile.shp",overwrite=TRUE)

# check this
surv.map2<-st_read("shapefiles/ECOMEM_Survey_Shapefile.shp")

stjoebay<-st_read("odata/stjoebay.shp")
standrewbay<-st_read("odata/standrewbay.shp")
floridasites<-st_read("odata/florida_sites.shp")

plot(surv.map2)

ggplot()+
  geom_sf(data=floridasites,fill="lightgrey")+
  geom_sf(data=surv.map2$geometry)


# get central coordinates for each plot for the experiment
exp.coords2<-exp.coords%>%
  group_by(blockID)%>%
  summarize(lat=mean(Lat),
            long=mean(Long))

exp.may24<-filter(exp.pc2,sampling=="s1")%>%
  left_join(exp.sd2)%>%
  left_join(exp.coords2)%>%
  mutate(siteID=paste0("ECOMEM.",blockID))%>%
  ungroup()%>%
  dplyr::select(siteID,lat,long,Tt_pc,Hw_pc,Sf_pc,Tt_sd,Hw_sd,Sf_sd)

exp.june24<-filter(exp.pc2,sampling=="s2")%>%
  left_join(exp.sd2)%>%
  left_join(exp.coords2)%>%
  mutate(siteID=paste0("ECOMEM.",blockID))%>%
  ungroup()%>%
  dplyr::select(siteID,lat,long,Tt_pc,Hw_pc,Sf_pc,Tt_sd,Hw_sd,Sf_sd)

exp.nov24<-filter(exp.pc2,sampling=="s3")%>%
  left_join(exp.sd2)%>%
  left_join(exp.coords2)%>%
  mutate(siteID=paste0("ECOMEM.",blockID))%>%
  ungroup()%>%
  dplyr::select(siteID,lat,long,Tt_pc,Hw_pc,Sf_pc,Tt_sd,Hw_sd,Sf_sd)


exp.shape.may<-exp.may24
coordinates(exp.shape.may)=~long+lat
proj4string(exp.shape.may)=CRS("+proj=longlat +datum=WGS84")

exp.shape.june<-exp.june24
coordinates(exp.shape.june)=~long+lat
proj4string(exp.shape.june)=CRS("+proj=longlat +datum=WGS84")

exp.shape.nov<-exp.nov24
coordinates(exp.shape.nov)=~long+lat
proj4string(exp.shape.nov)=CRS("+proj=longlat +datum=WGS84")

plot(exp.shape.may)
plot(exp.shape.june)
plot(exp.shape.nov)


raster::shapefile(exp.shape.may, "shapefiles/ECOMEM_Experiment_May2024_Shapefile.shp",overwrite=TRUE)
raster::shapefile(exp.shape.june, "shapefiles/ECOMEM_Experiment_June2024_Shapefile.shp",overwrite=TRUE)
raster::shapefile(exp.shape.nov, "shapefiles/ECOMEM_Experiment_November2024_Shapefile.shp",overwrite=TRUE)

# check this
expmay.map2<-st_read("shapefiles/ECOMEM_Experiment_May2024_Shapefile.shp")
expjune.map2<-st_read("shapefiles/ECOMEM_Experiment_June2024_Shapefile.shp")
expnov.map2<-st_read("shapefiles/ECOMEM_Experiment_November2024_Shapefile.shp")

ggplot()+
  geom_sf(data=floridasites,fill="lightgrey")+
  geom_sf(data=expjune.map2$geometry,color=expjune.map2$Tt_pc)
