# organize smaller seagrass data set for prop scar paper

# load custom script to script to check if a package is installed, install it if necessary, then load it
source("scripts/install_packages_function.R")

# load the packages we need for this script
lp("tidyverse")
lp("readxl")

prop<-read_xlsx("odata/PROP Site Scar Counts and Lengths .xlsx",sheet=2)%>%
  filter(scar.quantity.100m!="NA")%>%
  mutate(imagery.month=as.numeric(imagery.month),
         im.date=my(paste(imagery.month,imagery.year)))

# now bring in the seagrass biomass, growth, and nutrients to build the smaller
# data set

sg.lab<-read_xlsx("odata/SURVEY - Seagrass Lab Data.xlsx",sheet=2)
sg.lab2<-read_xlsx("odata/SURVEY - Seagrass Lab Data.xlsx",sheet=3)
sg.cnp<-read_xlsx("odata/SURVEY - Erin_ECOMEM_CNP.xlsx",sheet=2)
sg.cnpID<-read_xlsx("odata/SURVEY - Sample ID Key.xlsx",sheet=2)

glimpse(sg.lab)
glimpse(sg.lab2)
glimpse(sg.cnp)
glimpse(sg.cnpID)

# this data will take some organizing 
# start with sg.lab

sg.labb<-sg.lab%>%
  mutate(sample.weight=as.numeric(sample.weight))%>%
  separate(collect.date,into=c("cmnth","cd","cyr"))%>%
  separate(mark.date,into=c("mmnth","md","myr"))%>%
  mutate(collect.ymd=ymd(paste(cyr,cmnth,cd)),
         mark.ymd=ymd(paste(myr,mmnth,md)))%>%
  select(siteID,taxa=species,sample.type,mark.ymd,collect.ymd,sample.weight)

# we'll get new growth biomass as well as total above ground biomass

sg.newg<-sg.labb%>%
  filter(sample.type=="newgrowth")%>%
  filter(taxa=="Tt")%>%
  mutate(days.grow=collect.ymd-mark.ymd,
         biom.per.day=round((sample.weight/.018)/as.numeric(days.grow)),2)%>%# changes units to g biomass per m2 per day
  select(siteID,biom.per.day)

# now get total biomass
sg.totg<-sg.labb%>%
  filter(sample.type!="epiphytes")%>%
  group_by(siteID,taxa)%>%
  summarize(taxa.biom=round(sum(sample.weight,na.rm = T)/.018,2))%>%# changes units to g biomass per m2
  mutate(taxa=paste0(taxa,".biom"))%>%
  pivot_wider(names_from=taxa,values_from = taxa.biom,values_fill=0)%>%
  left_join(sg.newg)

# now get epiphyte biomass
sg.totg<-sg.labb%>%
  filter(sample.type=="epiphytes")%>%
  group_by(siteID)%>%
  summarize(epi.biom=round(sum(sample.weight,na.rm = T)/.018,2))%>%# changes units to g biomass per m2
  right_join(sg.totg)%>%
  mutate(ep.sg=epi.biom/Tt.biom)

# the epiphyte biomass does not look right. 
# get samples that are over 50%
sg.epitot<-filter(sg.totg,ep.sg>=.5)

# figure out whats going on here before moving on.
# now organize the morphometrics
