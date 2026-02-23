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
pts$col2<-paste(pts$yr2,pts$ID)
la.close<-st_crop(la,xmin=-81,
                  xmax=-62,
                  ymin=9.5,
                  ymax=27.5)

theme_set(theme_bw()+theme(panel.grid = element_blank()))


pts$col2<-factor(pts$col2,levels=c("2002 1",
                                    "2006 2",
                                    "2007 3",
                                    "2008 3",
                                    "2010 2",
                                   "2010 4",
                                   "2010 5",
                                    "2011 2",
                                    "2012 2",
                                    "2013 2",
                                    "2013 6",
                                    "2013 7",
                                    "2016 8",
                                    "2024 9",
                                    "2024 10",
                                    "2025 *"))
pts<-pts[order(pts$Yr),]
     
x<-c(bquote(.(pts$Yr[1])^.(pts$ID[1])),
     bquote(.(pts$Yr[2])^.(pts$ID[2])),
     bquote(.(pts$Yr[3])^.(pts$ID[3])),
     bquote(.(pts$Yr[4])^.(pts$ID[4])),
     bquote(.(pts$Yr[5])^.(pts$ID[5])),
     bquote(.(pts$Yr[6])^.(pts$ID[6])),
     bquote(.(pts$Yr[8])^.(pts$ID[8])),
     bquote(.(pts$Yr[9])^.(pts$ID[9])),
     bquote(.(pts$Yr[13])^.(pts$ID[13])),
     bquote(.(pts$Yr[14])^.(pts$ID[14])),
     bquote(.(pts$Yr[15])^.(pts$ID[15])),
     bquote(.(pts$Yr[17])^.(pts$ID[17])),
     bquote(.(pts$Yr[19])^.(pts$ID[19])),
     bquote(.(pts$Yr[20])^.(pts$ID[20])),
     bquote(.(pts$Yr[21])^.(pts$ID[21])))

(la.small<-ggplot(la.close)+
  geom_sf(fill="grey")+
  geom_point(aes(x=Longitude,y=Latitude,fill=col2),data=pts%>%filter(shp!=23),size=2.5,pch=21)+
    geom_point(aes(x=Longitude,y=Latitude,fill=col2),data=pts%>%filter(shp==23),size=2.5,pch=23)+
  scale_fill_viridis_d(name="Year first observed",option="H",labels=x)+
  guides(fill = guide_legend(ncol = 2))+
  #geom_text(aes(x=Longitude,y=Latitude,label = ref2),data=pts,nudge_x = 0.5,nudge_y=0.5)+
  ylab("")+
  xlab(""))

ggsave(plot=la.small,
       filename="figures/hsinvasion_map.png",width=6,height=7,dpi=500)

