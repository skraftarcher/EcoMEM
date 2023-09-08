# make maps to visualize survey data

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
datascripts<-list.files(path="scripts",pattern="*-EX.R")
datascripts<-datascripts[-grep(pattern="shapefiles",datascripts)]
for(i in 1:length(datascripts))source(paste0("scripts/",datascripts[i]))
stjoebay<-st_read("odata/stjoebay.shp")
standrewbay<-st_read("odata/standrewbay.shp")
floridasites<-st_read("odata/florida_sites.shp")

# organize data
# remove or rename columns that are labeled the same across datasets
# but should/may have different values
# also organize so there is one column for each variable of interest

site.coords<-select(water,date,siteID,lat,long)# pull out coordinates so
# they can be added to each type of data if necessary
  
water2<-water%>%
  select(siteID,date,ph:tds)

# summarize by site
urchin_size2<-urchin_size%>%
  select(siteID,date,size)%>%
  group_by(siteID,date)%>%
  summarize(m.urchin.size=mean(size,na.rm = TRUE),
            min.urchin.size=min(size,na.rm=TRUE),
            max.urchin.size=max(size,na.rm=TRUE))

urchin_count2<-urchin_count%>%
  select(siteID,date,urchin.count=tally)

# organize so each seagrass taxa has its own column for 
# mean shoot density and standard deviation, making 0s count
shtd.T<-shtd%>%
  select(siteID,date,quadrat,taxa,shoot.density)%>%
  filter(taxa=="T")%>%
  pivot_wider(names_from = quadrat,values_from = shoot.density,values_fill = 0)%>%
  pivot_longer("1":"4",names_to = "quadrat",values_to = "shoot.density")%>%
  group_by(siteID,date,taxa)%>%
  summarize(m.shtd=mean(shoot.density/(.1*.1),na.rm = TRUE),
            sd.shtd=sd(shoot.density/(.1*.1),na.rm=TRUE))

shtd.H<-shtd%>%
  select(siteID,date,quadrat,taxa,shoot.density)%>%
  filter(taxa=="H")%>%
  pivot_wider(names_from = quadrat,values_from = shoot.density,values_fill = 0)%>%
  pivot_longer("1":"4",names_to = "quadrat",values_to = "shoot.density")%>%
  group_by(siteID,date,taxa)%>%
  summarize(m.shtd=mean(shoot.density/(.1*.1),na.rm = TRUE),
            sd.shtd=sd(shoot.density/(.1*.1),na.rm=TRUE))

shtd.S<-shtd%>%
  select(siteID,date,quadrat,taxa,shoot.density)%>%
  filter(taxa=="S")%>%
  pivot_wider(names_from = quadrat,values_from = shoot.density,values_fill = 0)%>%
  pivot_longer("1":"4",names_to = "quadrat",values_to = "shoot.density")%>%
  group_by(siteID,date,taxa)%>%
  summarize(m.shtd=mean(shoot.density/(.1*.1),na.rm = TRUE),
            sd.shtd=sd(shoot.density/(.1*.1),na.rm=TRUE))

shtd.He<-shtd%>%
  select(siteID,date,quadrat,taxa,shoot.density)%>%
  filter(taxa=="He")%>%
  pivot_wider(names_from = quadrat,values_from = shoot.density,values_fill = 0)%>%
  pivot_longer("1":"4",names_to = "quadrat",values_to = "shoot.density")%>%
  group_by(siteID,date,taxa)%>%
  summarize(m.shtd=mean(shoot.density/(.1*.1),na.rm = TRUE),
            sd.shtd=sd(shoot.density/(.1*.1),na.rm=TRUE))

shtd.2<-bind_rows(shtd.T,shtd.H,shtd.S,shtd.He)%>%
  pivot_wider(names_from="taxa",values_from = c(m.shtd,sd.shtd),values_fill = 0)

# organize so each taxa has its own column for 
# mean percent cover and standard deviation, making 0s count

pc2<-pc%>%
  filter(!is.na(quadrat))%>%
  select(date,siteID,quadrat,taxa,percent.cover)%>%
  mutate(percent.cover=(percent.cover/25)*100)%>%
  pivot_wider(names_from = quadrat,values_from=percent.cover,values_fill = 0)%>%
  pivot_longer("1":"4",names_to = "quadrat",values_to="pc")%>%
  group_by(date,siteID,taxa)%>%
  summarize(pc.m=mean(pc),
            pc.sd=sd(pc))

# pull out only seagrass to map
pc.sg<-pc2%>%
  filter(taxa %in% c("T","H","S","He"))%>%
  pivot_wider(names_from=taxa,values_from=c(pc.m,pc.sd),values_fill=0)

# make a community data frame to get macrophyte species richness and diversity
pc.com<-pc2%>%
  select(-pc.sd)%>%
  pivot_wider(names_from = taxa,values_from = pc.m,values_fill = 0)%>%
  select(-"NA",-`T`,-H,-S,-He)

pc.com$alg.spr<-specnumber(pc.com[,-1:-2])
pc.com$alg.spdiv<-diversity(pc.com[,-1:-2])

plants<-select(pc.com,siteID,date,alg.spr,alg.spdiv)%>%
  left_join(pc.sg)%>%
  right_join(shtd.2)

# organize canopy data
can2<-canopy%>%
  select(-notes,-QAQC)%>%
  pivot_longer(canopy1:canopy4,names_to="quadratid",values_to="canopy")%>%
  mutate(canopy=ifelse(is.na(canopy),0,canopy),
         grazing=ifelse(grazing=="n",0,1))%>%
  group_by(date,siteID)%>%
  summarize(m.canopy=mean(canopy),
            sd.canopy=sd(canopy),
            grazed=ifelse(sum(grazing,na.rm=TRUE)>0,"yes","no"))%>%
  filter(date!="")


# make invert data set that includes species richness and diversity
# sum across quadrats for this one
inverts2<-inverts%>%
  filter(!is.na(quadrat))%>%
  select(date,siteID,taxa,abundance)%>%
  group_by(date,siteID,taxa)%>%
  summarize(abund=sum(abundance,na.rm=TRUE))%>%
  pivot_wider(names_from = taxa,values_from = abund,values_fill=0)

inverts2$inv.spr<-specnumber(inverts2[,-1:-2])
inverts2$inv.spdiv<-diversity(inverts2[,-1:-2])

inverts3<-select(inverts2,siteID,date,inv.spr,inv.spdiv)

# summarize propeller scar data
propscar2<-propscar%>%
  mutate(length.shore2=ifelse(length.shore==">20",1,0),
         length.water2=ifelse(
           length.water %in% c(">2",">9.67",">0",">4.8"),1,0),
         seagrass.regrow2=ifelse(seagrass.regrow%in%c("Y","y"),1,0),
         cut.rizhomes2=ifelse(cut.rizhomes%in%c("Y","y"),1,0))%>%
  filter(width+depth!=0)%>%
  group_by(date,siteID)%>%
    summarize(prop.scars=n(),
              prop.scars.over20m=sum(length.shore2+length.water2),
              scars.seagrass.regrowing=sum(seagrass.regrow2,na.rm=T),
              scars.cut.rhizomes=sum(cut.rizhomes2,na.rm=T))

# now link everything of interest to spatial data and fill in zeros
full.ds<-left_join(site.coords,plants)%>%
  left_join(can2)%>%
  left_join(inverts3)%>%
  left_join(urchin_count2)%>%
  left_join(urchin_size2)%>%
  left_join(propscar2)%>%
  filter(siteID!="SJB303")%>%
  mutate(across(alg.spr:sd.canopy,~ifelse(is.na(.x),0,.x)),
         across(inv.spr:scars.cut.rhizomes,~ifelse(is.na(.x),0,.x)),
         grazed=ifelse(is.na(grazed),"no",grazed))


# start making maps
# start with only sjb
theme_set(theme_bw()+theme(panel.grid=element_blank(),
                           axis.title = element_blank()))
# map may surveys, then summer surveys, then think about plotting differences
full.ds$date<-mdy(full.ds$date)

may.ds<-filter(full.ds,date<ymd("2023-06-01"))
summer.ds<-filter(full.ds,date>ymd("2023-06-01"))

sjb.ds<-full.ds%>%
  filter(lat<29.9)

#all variables sjb
pcln<-colnames(sjb.ds)
for(i in 5:length(pcln)){
  if(!is.character(sjb.ds[,colnames(sjb.ds)==pcln[i]])){
    ggplot()+
    geom_sf(data=stjoebay)+
    geom_point(data=sjb.ds,
               aes(y=lat,x=-long,color=sjb.ds[,colnames(sjb.ds)==pcln[i]]),
               size=3,alpha=.8)+
    ggtitle("Summer 2023")+
    scale_color_viridis_c(option="D",begin=.2,end=.8,name=pcln[i])
  ggsave(paste0("figures/sjb/",pcln[i],".jpg"),width=8,height=10)}
  if(is.character(sjb.ds[,colnames(sjb.ds)==pcln[i]])){
    ggplot()+
      geom_sf(data=stjoebay)+
      geom_point(data=sjb.ds,
                 aes(y=lat,x=-long,color=sjb.ds[,colnames(sjb.ds)==pcln[i]]),
                 size=3,alpha=.8)+
      ggtitle("Summer 2023")+
      scale_color_viridis_d(option="D",begin=.2,end=.8,name=pcln[i])
    ggsave(paste0("figures/sjb/",pcln[i],".jpg"),width=8,height=10)
    }
}


sab.ds<-full.ds%>%
  filter(lat>29.9)

#all variables sab
pcln<-colnames(sab.ds)
for(i in 5:length(pcln)){
  if(!is.character(sab.ds[,colnames(sab.ds)==pcln[i]])){
    ggplot()+
      geom_sf(data=standrewbay)+
      geom_point(data=sab.ds,
                 aes(y=lat,x=-long,color=sab.ds[,colnames(sab.ds)==pcln[i]]),
                 size=3,alpha=.8)+
      ggtitle("Summer 2023")+
      scale_color_viridis_c(option="D",begin=.2,end=.8,name=pcln[i])
    ggsave(paste0("figures/sab/",pcln[i],".jpg"),width=10,height=8)}
  if(is.character(sab.ds[,colnames(sab.ds)==pcln[i]])){
    ggplot()+
      geom_sf(data=standrewbay)+
      geom_point(data=sab.ds,
                 aes(y=lat,x=-long,color=sab.ds[,colnames(sab.ds)==pcln[i]]),
                 size=3,alpha=.8)+
      ggtitle("Summer 2023")+
      scale_color_viridis_d(option="D",begin=.2,end=.8,name=pcln[i])
    ggsave(paste0("figures/sab/",pcln[i],".jpg"),width=10,height=8)
  }
}


