# organize larger seagrass data for prop scar paper

# load custom script to script to check if a package is installed, install it if necessary, then load it
source("scripts/install_packages_function.R")

# load the packages we need for this script
lp("tidyverse")
lp("readxl")
lp("vegan")
lp("chemodiv")


# the seagrass data is all in one workbook, this script brings in each sheet
# and assigns that sheet to a dataframe object (e.g., the pc, sd, can)
pc<-read_xlsx("odata/SURVEY - Macrophyte Community.xlsx",sheet=2)

sd<-read_xlsx("odata/SURVEY - Macrophyte Community.xlsx",sheet=4)

can<-read_xlsx("odata/SURVEY - Macrophyte Community.xlsx",sheet=5)

# I'm also going to bring in one of the prop scar datasets - 
# this will give me the sites we need - because 0s count here 

prop<-read_xlsx("odata/PROP Site Scar Counts and Lengths .xlsx",sheet=2)%>%
  filter(scar.quantity.100m!="NA")%>%
  mutate(imagery.month=as.numeric(imagery.month),
         im.date=my(paste(imagery.month,imagery.year)))

# look at each data set to make sure it brought in what I wanted it to bring in

glimpse(pc)
glimpse(sd)
glimpse(can)
glimpse(prop)

# they look good
# now we can start organizing
# first thing I'm going to do is pull out the surveys closest to the imagery date
# for the sites that were surveyed twice

# first get the sites that were surveyed twice

pctwice<-pc%>%
  select(siteID,date)%>% # only keep these two columns
  distinct()%>% # only keep distinct siteID and date combinations
  group_by(siteID)%>% # group observations by siteID
  summarize(ns=n())%>% #get the number of rows for each site
  filter(ns>=2)%>% # only keep sites that were surveyed twice
  left_join(pc[,c(1,3)])%>% # bring the survey dates back in
  distinct()%>% # make sure we only keep distinct rows
  left_join(prop)%>% # join the prop scar data set on
  mutate(diff.si=abs(ymd(date)-im.date))%>% #get the absolute difference between the imagery date and the survey date
  group_by(siteID)%>% # regroup by siteID
  mutate(min.diff=min(diff.si))%>%# find the minimum difference for each site
  filter(diff.si!=min.diff)%>%#filter down to only keep the survey date that was NOT the closest to the imagery date
  select(siteID,date)# keep only the siteID and date

# the reason we kept only the ones above that aren't the ones we'll want to use 
# is that its easier to filer out these observations

# second thiprop# second thing I'm going to to is spread the data out wide to add 0s where there should
# be then make the dataset "long" again

pc2<-pc%>%
  filter(!paste(siteID,date) %in% paste(pctwice$siteID,pctwice$date))%>% # filter out the surveys that were furthest away from the imagery date
  filter(siteID %in% prop$siteID)%>% #filter down to only sites in the prop scar dataset
  select(date,surveyor,siteID,quadrat,taxa,percent.cover)%>%#select only the columns I need
  pivot_wider(names_from = taxa,values_from=percent.cover,values_fill=0)%>% #this makes the dataset wider
  pivot_longer('T':ACE,names_to="taxa",values_to = "pc")%>%# this makes the dataset longer again - the first bit is which columns we want to transpose - the T has to be in '' because in R language T is equivalent to TRUE
  # notice the dataset has a lot more rows now - that is because there are rows now for each taxa we saw at all sites - most are 0s
  group_by(date,siteID,taxa)%>% # now I am telling R that rows with the same date, SiteID, and taxa values belong to the same group
  summarize(pc.m=round(mean(pc/25),2),#get the average % cover for each taxa, at each site, at each sampling
            pc.sd=round(sd(pc/25),2)) #get the variability - or standard deviation

# now we can separate these into seagrass and macroalgae - 
# the seagrass taxa are T,S,H, and He

pc.sg<-filter(pc2,taxa %in% c("T","S","H","He")) # this says only keep rows where the value of taxa is in that list
pc.alg<-filter(pc2, !taxa %in% c("T","S","H","He")) #the ! before taxa in this line says only keep rows where taxa is not in the list

# now I am going to rename T so it doesn't have a name that conflicts with a base R command
# then I'm going to spread the dataset wide again - so we should have one row per site/sampling

pc.sg<-pc.sg%>%
  mutate(taxa=ifelse(taxa=="T","Tt",taxa))%>%# this line says I want to create or change a variable - then I want to work with taxa, and if taxa is equal to T I want taxa to be equal to Tt otherwise I want it to keep the value it had
  pivot_wider(names_from = taxa,values_from=c(pc.m,pc.sd))

# do the same thing with algae - but in this case we're only going to
# care about algae taxa richness and diversity so I'm not going to bother with the sd

pc.alg<-pc.alg%>%
  select(-pc.sd)%>%
  pivot_wider(names_from = taxa,values_from=pc.m)

# now we can get things like taxa richness and evenness

pc.sg$sg.rich<-specnumber(pc.sg[,3:6]) # calculate species richness
pc.sg$alg.rich<-specnumber(pc.alg[,-1:-2]) # calculate taxa richness
pc.sg$sg.even<-calcDiv(pc.sg[,3:6],type="PielouEven")$PielouEven # calculate pielou's evenness
pc.sg$alg.even<-calcDiv(pc.alg[,-1:-2],type="PielouEven")$PielouEven # calculate pielou's evenness

# now organize shoot density data to add onto this data set

# again, remove sampling dates that we're not using
sd2<-sd%>%
  filter(!paste(siteID,date) %in% paste(pctwice$siteID,pctwice$date))%>% # filter out the surveys that were furthest away from the imagery date
  filter(siteID %in% prop$siteID)%>%
  select(date,siteID,taxa,quadrat,shoot.density)%>%
  mutate(taxa=ifelse(taxa=="T","Tt",taxa),
         shoot.density=as.numeric(shoot.density),
         shoot.density=ifelse(is.na(shoot.density),0,shoot.density),
         shoot.density=shoot.density/.01)%>%
  pivot_wider(names_from = taxa,values_from = shoot.density,values_fill = 0)%>%
  pivot_longer(Tt:He,names_to="taxa",values_to="sht.d")%>%
  group_by(date,siteID,taxa)%>%
  summarize(sht.d.m=round(mean(sht.d),2),
            sht.d.sd=round(sd(sht.d),2))%>%
  pivot_wider(names_from = taxa,
              values_from = c(sht.d.m,sht.d.sd))

pc.sg<-left_join(pc.sg,sd2)


# now work on canopy height
can2<-can%>%
  filter(!paste(siteID,date) %in% paste(pctwice$siteID,pctwice$date))%>% # filter out the surveys that were furthest away from the imagery date
  filter(siteID %in% prop$siteID)%>%
  filter(canopy1!="NA")%>%
  mutate(canopy1=as.numeric(canopy1),
         canopy2=as.numeric(canopy2),
         canopy3=as.numeric(canopy3),
         canopy4=as.numeric(canopy4))%>%
  select(-surveyor,-notes,-QAQC,-grazing)%>%
  pivot_longer(canopy1:canopy4,names_to = "can",values_to = "can.ht")%>%
  group_by(date,siteID)%>%
  summarize(can.ht.m=mean(can.ht,na.rm = T),
            can.ht.sd=sd(can.ht,na.rm=T))

pc.sg<-left_join(pc.sg,can2)  

# that's all the seagrass survey info now we can start adding on prop scar info
# bring in the other pages of prop scar info
prop.lngh<-read_xlsx("odata/PROP Site Scar Counts and Lengths .xlsx",sheet=3)
prop.close<-read_xlsx("odata/PROP Site Scar Counts and Lengths .xlsx",sheet=4)

# clean up prop counts & closeness then join on to PC dataset
pc.sg<-left_join(pc.sg,prop[,1:4])%>%
  left_join(prop.close[,1:2])
  

# summarize lengths then add together
prop.lngh2<-prop.lngh%>%
  mutate(scar.length.full=as.numeric(scar.length.full),
         scar.length.clipped.100m=as.numeric(scar.length.clipped.100m),
         scar.length.clipped.50m=as.numeric(scar.length.clipped.50m),
         scar.length.clipped.10m=as.numeric(scar.length.clipped.10m),
         scar.length.full=ifelse(is.na(scar.length.full),0,scar.length.full),
         scar.length.clipped.100m=ifelse(is.na(scar.length.clipped.100m),0,scar.length.clipped.100m),
         scar.length.clipped.50m=ifelse(is.na(scar.length.clipped.50m),0,scar.length.clipped.50m),
         scar.length.clipped.10m=ifelse(is.na(scar.length.clipped.10m),0,scar.length.clipped.10m))%>%
  group_by(siteID)%>%
  summarize(scar.length.full=sum(scar.length.full),
            scar.length.clipped.100m=sum(scar.length.clipped.100m),
            scar.length.clipped.50m=sum(scar.length.clipped.50m),
            scar.length.clipped.10m=sum(scar.length.clipped.10m))

pc.sg<-left_join(pc.sg,prop.lngh2)%>%
  mutate(
    scar.length.full=ifelse(is.na(scar.length.full),0,scar.length.full),
    scar.length.clipped.100m=ifelse(is.na(scar.length.clipped.100m),0,scar.length.clipped.100m),
    scar.length.clipped.50m=ifelse(is.na(scar.length.clipped.50m),0,scar.length.clipped.50m),
    scar.length.clipped.10m=ifelse(is.na(scar.length.clipped.10m),0,scar.length.clipped.10m))

# last thing we might want is which bay each site is in. This is more complicated
# than it needs to be because of the added sites. 

pc.sg2<-pc.sg%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE) # this bit of code separates the text from the numbers in
#our siteIDs when the text comes before the number (i.e., pre-determined sites)

pc.sg.oth<-filter(pc.sg2,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))# this bit of code separates the text from the numbers in
#our siteIDs when the number comes before the text (i.e., sites that were moved in the field)
bay.ds<-pc.sg2%>% # 
  ungroup()%>% #ungroups the data so it doesn't try to add grouping variables back in
  filter(!is.na(num))%>% # removes the rows that will be added back in via prop.sum.oth
  bind_rows(pc.sg.oth)%>% # add these sites back in
  select(siteID,bay)%>%
  distinct()

# I'm going to save this so we can just use this dataset in the future if we need to

write.csv(bay.ds,"SURVEY - bay by site.csv",row.names = FALSE)

# now we can join this back on to the pc.sg dataset

pc.sg<-left_join(pc.sg,bay.ds)

# ok I think we're good! 
# save this data set



write.csv(pc.sg,"wdata/PROP_combined_sg_prop.csv",row.names = FALSE)


