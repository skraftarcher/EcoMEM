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
  filter(siteID!="SAB74")%>% # the data from this site is not trustworthy
  filter(SampleID!="NA")%>% # get rid of rows without a sample ID
  mutate(sample.weight=as.numeric(sample.weight))%>%# the download process turned empty cells in to text NAs - these are stored differently in R than NAs that mean missing data in R
  # this causes the data that should be numeric to be stored as a character - which means we can't do any calculations with it
  # by forcing it to be a number (what as.numeric does) will change those back to the "no data" NAs
  separate(collect.date,into=c("cmnth","cd","cyr"))%>%# the way the dates were entered means R wont recognize them as a date. To fix this 
  # we can split it into the month, day, and year components. Usually you have to tell R where to separate - but a . is one of the separators R automatically recognizes
  separate(mark.date,into=c("mmnth","md","myr"))%>% # same deal here
  mutate(collect.ymd=ymd(paste(cyr,cmnth,cd)),# all that so we can make actual dates
         mark.ymd=ymd(paste(myr,mmnth,md)))%>% # you will get an error here - its where there wasn't a mark date- its ok
  select(siteID,taxa=species,sample.type,mark.ymd,collect.ymd,sample.weight)#only keep the rows we need/want

# we'll get new growth biomass as well as total above ground biomass
# we can look at growth 2 ways - through areal extension per day (i.e., the area of new leaf growth) and through biomass gained per day
# areal growth is on the next sheet in the excel file so we'll start with biomass
sg.newg<-sg.labb%>%
  filter(sample.type=="newgrowth")%>% # pull out only new growth samples
  filter(taxa=="Tt")%>%# even though there are halodule samples here it doesn't seem like any of them had markings so going down to only
  # Thalassia samples
  filter(!is.na(mark.ymd))%>%# gets rid of samples that don't have a mark date
  mutate(days.grow=collect.ymd-mark.ymd,# get the number of days of growth 
         biom.per.day=round((sample.weight/.018)/as.numeric(days.grow),4))%>%# changes units to g biomass per m2 per day
  select(siteID,biom.per.day)# only keep the columns we want moving forward

# now get total biomass
sg.totg<-sg.labb%>%
  filter(sample.type!="epiphytes")%>%# take out epiphytes - epiphytes both aren't seagrass and the data are questionable
  group_by(siteID,taxa)%>%# get taxa biomass separately
  summarize(taxa.biom=round(sum(sample.weight,na.rm = T)/.018,4))%>%# changes units to g biomass per m2
  mutate(taxa=paste0(taxa,".biom"))%>%# add biom to the end of the taxa abbreviations so the column names are clear when we pivot wider
  pivot_wider(names_from=taxa,values_from = taxa.biom,values_fill=0)%>%# takes the longer dataset and makes those taxa names columns so we still have 1 row per site
  left_join(sg.newg)# add on the new growth biomass

# We aren't using the epiphyte data so that's it for data organized from the biomass sheet
# now on to morphometrics and growth (areal extension )
# now organize the morphometrics
sg.morph<-sg.lab2%>%
  filter()
  mutate(new.length=as.numeric(new.length),# dealing with those NAs the same way we did for biomass
         width=as.numeric(width),
         grow.area=new.length*width,# calcualte the area of new growth
         all.area=total.length*width)%>%# calclulate area of the leaf
  group_by(siteID,species)%>%# group by site and species
  summarize(max.length=max(total.length,na.rm = T),# get the maximum leaf length
            max.width=max(width,na.rm=T),# get the maximum leaf width - turns out not all widths were recorded. For now we'll keep it and try to get at area 
            # but if there aren't enough for area then we can use linear extension 
            # you will get errors here its ok - its those sites where no widths were recorded
            total.area=sum(all.area,na.rm=T),# get the total area
            grow.length=sum(new.length,na.rm=T),# get the total length of new growth
            grow.area=sum(grow.area,na.rm=T))%>%# get the total area of new growth
  mutate(total.area=ifelse(max.width==-Inf,NA,total.area),# turn areas that can't be calculated into NAs
        grow.area=ifelse(max.width==-Inf,NA,grow.area))# turn areas that can't be calculated into NAs

# now organize cnp data
sg.cnp2<-left_join(sg.cnp,sg.cnpID[,-4])%>%# join the cnp data with the dataset with the IDs that link it to site, sample type, etc
  mutate(PP=as.numeric(PP),# do that same trick to turn missing data into real NAs
         sample.type=case_when( # sample types are not consistent between this data set and the biomass dataset, but we will want 
           # to join the two. So fixing that now
           sample.type=="NA"~"epiphytes",
           sample.type=="UM"~"unmarked",
           sample.type=="OG"~"oldgrowth",
           sample.type=="NG"~"newgrowth",
           sample.type=="UM/H"~"unmarked"))%>%
  filter(!grepl("H",sampleID))%>%#the halodule samples aren't marked in any way as different other than having an H at the end of the sampleID - because there are so few of these
  # I'm just going to get rid of them
  select(-sampleID)%>%# get rid of sample ID because - while they should match the biomass data I'm not totally sure they would
  group_by(siteID,sample.type,bay)%>%# some samples were run mutltiple times - taking the average of those to use going forward
  summarize(PN=mean(PN,na.rm=T)/100,
            PC=mean(PC,na.rm=T)/100,
            PP=mean(PP,na.rm = T)/100)

# C,N, and P were run on each component of the seagrass (old growth, new growth, and unmarked growth). This isn't normal practice if you
# aren't trying to answer specific questions about differences between new and old growth
# we aren't - so to get carbon, nitrogen, and phosphorus for all seagrass  we need to multiply the % of each of these by the total weight
# of each compartment - then we can add those teogether, then divide by the total seagrass biomass for that site to get %ages comparable to other studies
sg.cnp3<-left_join(sg.labb,sg.cnp2)%>%
  mutate(tc=sample.weight*PC,
         tn=sample.weight*PN,
         tp=sample.weight*PP)

cnp<-sg.cnp3%>%
  filter(sample.type!="epiphytes")%>%
  group_by(siteID,taxa)%>%
  filter(taxa=="Tt")%>%
  summarize(tn=sum(tn,na.rm = T),
            tp=sum(tp,na.rm = T),
            tc=sum(tc,na.rm=T),
            tweight=sum(sample.weight,na.rm=T))%>%
  mutate(PN=(tn/tweight)*100,
         PP=(tp/tweight)*100,
         PC=(tc/tweight)*100)%>%
  select(-tn,-tp,-tc,-tweight)

# now that we have % c, n, and p that are comparable to other studies we can add this back on the growth and biomass datasets
sg.lab.final<-left_join(sg.totg,sg.morph)%>%
  left_join(cnp)%>%
  filter(species=="Tt")%>%
  select(-Hw.biom)%>%
  # last thing we're going to do - turn growth lengths that are 0 but growth biomass and everything else related to growth is NA
  # to NA
  mutate(grow.length=ifelse(is.na(biom.per.day),NA,grow.length),
         grow.area=ifelse(is.na(biom.per.day),NA,grow.area))
# doing that last bit exposed a site with new growth biomass but no new growth length... trying to figure that out but for now - don't use seagrass growth length or area
write.csv(sg.lab.final)
         