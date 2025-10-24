# make BPC site figure

library(tidyverse)
library(ggmap)
library(readxl)
if(!require(sf))install.packages("sf");library(sf)
if(!require(sp)) install.packages('sp');library(sp)
if(!require(ggspatial)) install.packages('ggspatial');library(ggspatial)


# load data and shapefiles----
pts<-read_xlsx("odata/hs_spread.xlsx")
la<-read_sf("C:\\Users\\sarcher\\Documents\\shapefiles\\land_polygons.shp")
sf_use_s2(FALSE)
pts$yr2<-factor(pts$Yr)
pts$ref2<-as.numeric(factor(pts$Ref))
la.close<-st_crop(la,xmin=-81,
                  xmax=-62,
                  ymin=9.5,
                  ymax=27.5)

theme_set(theme_bw()+theme(panel.grid = element_blank()))

(la.small<-ggplot(la.close)+
  geom_sf(fill="grey")+
  geom_point(aes(x=Longitude,y=Latitude,fill=yr2),data=pts%>%filter(shp!=23),size=2.5,pch=21)+
    geom_point(aes(x=Longitude,y=Latitude,fill=yr2),data=pts%>%filter(shp==23),size=2.5,pch=23)+
  scale_fill_viridis_d(name="Year first observed",option="H")+
  #geom_text(aes(x=Longitude,y=Latitude,label = ref2),data=pts,nudge_x = 0.5,nudge_y=0.5)+
  ylab("")+
  xlab(""))

ggsave(plot=la.small,
       filename="figures/hsinvasion_map.png",width=5,height=7,dpi=500)

