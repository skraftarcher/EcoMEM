# make maps of experiment sites

# Stephanie K. Archer 

# load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("ggspatial")
lp("sf")
lp("rgdal")
lp("sp")
lp("vegan")
lp("lubridate")
lp("patchwork")

# download/load data
source("scripts/download_propscar_experiment-EX.R")

stjoebay<-st_read("odata/stjoebay.shp")
standrewbay<-st_read("odata/standrewbay.shp")
floridasites<-st_read("odata/florida_sites.shp")

#organize data
sites.all<-filter(pscar,Site.ID!="")
sjb<-filter(sites.all,Bay=="SJB")
sab<-filter(sites.all,Bay=="SAB")

# make cropped sjb shapefile
sjb.box<-data.frame(ymin=min(sjb$Latitude)-.01,
                    ymax=max(sjb$Latitude)+.01,
                    xmax=min(sjb$Longitude)-.01,
                    xmin=max(sjb$Longitude)+.01)

sjb.close<-st_crop(stjoebay,xmin=sjb.box$xmin,xmax=sjb.box$xmax,
                   ymin=sjb.box$ymin,ymax=sjb.box$ymax)

# start making maps
# start with only sjb
theme_set(theme_bw()+theme(panel.grid=element_blank(),
                           axis.title = element_blank()))

(sjbb<-ggplot()+
  geom_sf(data=stjoebay,fill="lightgrey")+
  geom_rect(data=sjb.box,aes(xmin=xmin,ymax=ymax,xmax=xmax,ymin=ymin),fill=NA,color="red")+
  geom_point(data=sjb,aes(x=Longitude,y=Latitude),size=2)+
    scale_x_continuous(breaks=c(-85.42,-85.38,-85.34,-85.300)))

(sjbc<-ggplot()+
  geom_sf(data=sjb.close,fill="lightgrey")+
    geom_label(data=sjb,aes(x=Longitude,y=Latitude,label=Site.ID),
              nudge_x=c(-.0025,0,rep(-0.0025,4)),
              nudge_y=c(0,-0.0015,rep(0,4)))+
  geom_point(data=sjb,aes(x=Longitude,y=Latitude),size=2)+
  scale_x_continuous(breaks=c(-85.415,-85.400))+
    scale_y_continuous(breaks=c(29.77,29.79,29.81,29.83)))

sjbb+sjbc

ggsave("figures/stjoebay_experiment_sites.jpg",width=13,height=9,dpi=500)


#st andrew bay
sab.box<-data.frame(ymin=min(sab$Latitude)-.01,
                    ymax=max(sab$Latitude)+.01,
                    xmax=min(sab$Longitude)-.01,
                    xmin=max(sab$Longitude)+.01)

sab.close<-st_crop(standrewbay,xmin=sab.box$xmin,xmax=sab.box$xmax,
                   ymin=sab.box$ymin,ymax=sab.box$ymax)


(sabb<-ggplot()+
    geom_sf(data=standrewbay,fill="lightgrey")+
    geom_rect(data=sab.box,aes(xmin=xmin,ymax=ymax,xmax=xmax,ymin=ymin),fill=NA,color="red")+
    geom_point(data=sab,aes(x=Longitude,y=Latitude),size=2)+
    scale_x_continuous(breaks=c(-85.85,-85.75,-85.65,-85.55))+
    scale_y_continuous(breaks=c(30.05,30.15,30.25,30.35)))

(sabc<-ggplot()+
    geom_sf(data=sab.close,fill="lightgrey")+
    geom_label(data=sab,aes(x=Longitude,y=Latitude,label=Site.ID),
               nudge_x=-0.0025)+
    geom_point(data=sab,aes(x=Longitude,y=Latitude),size=2)+
    scale_x_continuous(breaks=c(-85.68,-85.66,-85.64))+
    scale_y_continuous(breaks=c(30.115,30.130,30.145)))

sabb+sabc

ggsave("figures/standrewbay_experiment_sites.jpg",width=13,height=8,dpi=500)
