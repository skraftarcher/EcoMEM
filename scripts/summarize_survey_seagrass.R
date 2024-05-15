# this script is summarize seagrass data from surveys

# Stephanie K. Archer 09/20/2023


# load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("kableExtra")
lp("matrixStats")
lp("vegan")

#bring in data
source("scripts/download_propeller_scar_survey-EX.R")
source("scripts/download_percentcover_survey-EX.R")
source("scripts/download_canopy_survey-EX.R")
source("scripts/download_shootdensity_survey-EX.R")
source("scripts/download_waterquality_survey-EX.R")
source("scripts/download_urchinsize_survey-EX.R")
source("scripts/download_urchincount_survey-EX.R")
source("scripts/download_sessileinverts_survey-EX.R")
source("scripts/download_labseagrass_biomass_survey-EX.R")
source("scripts/download_labseagrass_growth_survey-EX.R")

# organize/summarize data by bay
#propeller scars----
prop.sum<-propscar%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE) # this bit of code separates the text from the numbers in
#our siteIDs when the text comes before the number (i.e., pre-determined sites)

prop.sum.oth<-filter(prop.sum,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))# this bit of code separates the text from the numbers in
#our siteIDs when the number comes before the text (i.e., sites that were moved in the field)

bay.ds<-prop.sum%>% # start to calculate summary statistics
  filter(!is.na(num))%>% # removes the rows that will be added back in via prop.sum.oth
  bind_rows(prop.sum.oth)%>% # add these sites back in
  select(siteID,bay)%>%
  distinct()

# pause here to make a dataset of siteID and bay so that I don't have to repeat this code all the time
prop.sum2<-prop.sum%>% # start to calculate summary statistics
  filter(!is.na(num))%>% # removes the rows that will be added back in via prop.sum.oth
  bind_rows(prop.sum.oth)%>% # add these sites back in
  mutate(date=mdy(date),#turn date into an object stored as a date
         mnth=month(date),#get the month of the survey out of the date
         mnth=ifelse(mnth==5,5,6),# basically turn month into a sampled in May vs not sampled in may column
         length.20=ifelse(is.na(as.numeric(length.shore))|is.na(as.numeric(length.water)),"yes","no"),# prop scars longer than 20m were just recorded as longer than 20 m, this will let me calculate how many were longer
         length1=parse_number(length.shore),#pulls the number part out (i.e., gets rid of the greater than sign)
         length2=parse_number(length.water),#pulls the number part out (i.e., gets rid of the greater than sign)
         length=ifelse(length.20=="no",length1+length2,length1+length2+1),# adds the shoreward and waterward lenths together - adds one meter to scars taht were longer than 20m
         rhizomes=ifelse(cut.rizhomes=="Y",1,0),# turns this variable into 1 if yes so I can add to get a total # of scars with this property
         regrow=ifelse(seagrass.regrow=="Y",1,0))%>%# turns this variable into 1 if yes so I can add to get a total # of scars with this property
  group_by(date,siteID,bay)%>% # group by date, siteID, bay to calculate visit-specific means etc.
  mutate(total.length=sum(length))%>%# total length of scars per survey
  ungroup()%>% #remove grouping
  group_by(bay)%>%# group observations by region of bay
  mutate(n.site.visits=length(unique(paste0(date,siteID))),# number of surveys done (some sites were visited multiple times)
         n.sites=length(unique(siteID)))%>% #number of sites visited
  filter(length!=0)%>%# now that we know how many surveys we did we can get rid of sites/entries that are 0 length
  summarize(n.withprops=length(unique(paste0(date,siteID))),# number of surveys where prop scars were seen
            n.site.visits=n.site.visits,
            prop.withscars=n.withprops/n.site.visits,# proportion of surveys with prop scars
            n.prop.scars=n(),#total number of prop scars encountered
            mean.prop.scars=n.prop.scars/n.site.visits,#mean number of prop scars per survey
            ave.length=mean(length),# average length
            avg.width=mean(width,na.rm = T),#average width
            avg.depth=mean(depth,na.rm = T),#average depth
            min.length=min(length),#minimum length
            max.length=max(length),#maximum length
            prop.scars.with.cut.rhizomes=sum(rhizomes)/n.prop.scars,# proportion of prop scars with cut rhizomes
            prop.scars.with.seagrass.regrowth=sum(regrow)/n.prop.scars)%>%# proportion of prop scars with seagrass regrowth
  distinct()%>% # keep only unique rows
  mutate(max.length=ifelse(max.length>20,">20",as.character(max.length)))# change max lengths that are greater than 20 to >20

# make table
 
prop.sum3<-prop.sum2%>%
  ungroup()%>%
  select(Region=bay,# select only the columns I want in the table and name them nice names for a table
         `Number of surveys completed`=n.site.visits,
         `Proportion of sites with scars`=prop.withscars,
         `Mean number of scars per site`=mean.prop.scars,
         `Minimum scar length (m)`=min.length,
         `Maximum scar length (m)`=max.length,
         `Average scar width (m)`=avg.width,
         `Average scar depth (m)`=avg.depth,
         `Proportion of scars with cut rhizomes`=prop.scars.with.cut.rhizomes,
         `Proportion of scars with evidence of regrowth`=prop.scars.with.seagrass.regrowth)%>%
  mutate(across(c(3:5,7:10),~round(.x,2)),# round calculated columns to 2 decimal places
         Region=case_when(# change the regions from abbreviations to full names
           Region=="NB"~"North Bay",
           Region=="WB"~"West Bay",
           Region=="SJB"~"St. Joseph Bay",
           Region=="SAB"~"St. Andrew Bay"))
# set order of regions
prop.sum3$Region<-factor(prop.sum3$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
prop.sum3<-arrange(prop.sum3,Region)

#make table
prop.table<-kbl(prop.sum3,booktabs = T,align="c",
                caption="Summary of the propeller scars encountered during the permitted survey by Region. Propeller scars were surveyed along a 50m transect run parellel to shore.")%>%
  column_spec(1,width="1in",latex_valign = "m")%>%
  column_spec(2,width="1.4cm",latex_valign = "m")%>%
  column_spec(3,width="1.4cm",latex_valign = "m")%>%
  column_spec(4,width="1.4cm",latex_valign = "m")%>%
  column_spec(5,width="1.4cm",latex_valign = "m")%>%
  column_spec(6,width="1.4cm",latex_valign = "m")%>%
  column_spec(7,width="1.4cm",latex_valign = "m")%>%
  column_spec(8,width="1.4cm",latex_valign = "m")%>%
  column_spec(9,width=".6in",latex_valign = "m")%>%
  column_spec(10,width=".6in",latex_valign = "m")


# calculate average canopy height, percent cover, & shoot density 
# for seagrasses in each region for each month
#canopy height----
can.sum2<-canopy%>%
  left_join(bay.ds)%>%# join on the bay dataset
  pivot_longer(canopy1:canopy4,names_to = "meas",values_to="can.ht")%>%# make the dataset long to make math easier
  mutate(date=mdy(date),# make date a date object
         grazing=ifelse(grazing=="n",0,1))%>% # change grazing to 0 or 1 to make math easier
  group_by(date,siteID)# group by date and site ID

# I'm not confident that all quadrats had canopy heights recorded
# pulling out date, siteID, and quadrat from percent cover data to make sure
can.check<-can.sum2%>%
  select(date,siteID,quadrat)%>%
  mutate(has.canopy=1)%>%
  distinct()# check dataset that has only date, siteID, and quadrat #  and a 1 for yes it does have canopy

pc2<-pc%>%
  select(date,siteID,quadrat)%>%
  mutate(date=mdy(date))%>%
  distinct()%>%
  left_join(can.check)%>%
  filter(is.na(has.canopy))
# all sites/quadrats that had data recorded now have canopy rows
# There are 5 instances where canopy was not recorded fully for all quadrats and
# and the height cannot be inferred from percent cover data 
# (height was inferred as 0 when quadrat had no seagrass present or where
# one is missing and only one seagrass species was present at less than 50% cover)

can.sum3<-can.sum2%>%# start organizing for table
  mutate(mnth=month(date))%>% # extract the month from the date
  group_by(date,bay,mnth,siteID)%>% # group to calculate survey-specific values
  mutate(m.site.ht=mean(can.ht,na.rm = T),# average canopy height at each site, each time it was surveyed
            site.grazed=ifelse(sum(grazing,na.rm = T)==0,0,1))%>%#whether or not there was evidence of grazing at the site
  ungroup()%>% # reduce grouping, should now have 1 row per survey
  group_by(mnth,bay)%>%# group to summarize by month and region
  summarize(mean.ht=round(mean(m.site.ht),2),#average canopy height in the region rounded to 2 decimal points
            min.ht=min(can.ht,na.rm=T),#minimum canopy height in the region 
            max.ht=max(can.ht,na.rm=T),# maximum canopy height in the region
            proportion.grazed=round(sum(site.grazed)/n(),2))%>%# proportion of surveys were evidence of grazing was found
  mutate(mnth=case_when(# turn month numbers into nice column labels for table
    mnth==5~"May",
    mnth==6~"June",
    mnth==7~"July"),
    bay=case_when(# turn region abbreviations into nice column labels for table
      bay=="NB"~"North Bay",
      bay=="WB"~"West Bay",
      bay=="SJB"~"St. Joseph Bay",
      bay=="SAB"~"St. Andrew Bay"),
    ht.range=paste(mean.ht,"(",min.ht,"-",max.ht,")"))%>% # make a range character string for table
  select(-min.ht,-max.ht,-mean.ht)%>%# get rid of columns that aren't going in the table
  pivot_wider(names_from=mnth,values_from = c(ht.range,proportion.grazed))#make wider for display purposes

# set order of regions for table
can.sum3$Region<-factor(can.sum3$bay,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
can.sum3<-arrange(can.sum3,Region)

# change NAs to '-' for table
can.sum4<-can.sum3[,c(8,2:7)]%>%
  mutate(ht.range_July=ifelse(is.na(ht.range_July),"-",ht.range_July),
         proportion.grazed_July=ifelse(is.na(proportion.grazed_July),"-",proportion.grazed_July))

# set column names for the table
colnames(can.sum4)[2:7]<-rep(c("May","June","July"),2)

#make table
can.table<-kbl(can.sum4,booktabs=T,align="c",caption="Canopy heights (cm) of seagrasses (mean and range of values) and proportion of sites where there was evidence of grazing activity across the survey period. A '-' indicates that no surveys were conducted within that region during that month.")%>% 
  add_header_above(c("","Canopy height (cm)"=3," Proportion of sites Grazed"=3))%>%
  column_spec(1,width="1in")%>%
  column_spec(2,width="1.15in")%>%
  column_spec(3,width="1.15in")%>%
  column_spec(4,width="1.15in")%>%
  column_spec(5,width="0.7in")%>%
  column_spec(6,width="0.7in")%>%
  column_spec(7,width="0.7in")

# now make a table of % cover of seagrass and shoot densities
#pc and shd----
pc2<-pc%>%# this creates a dataset with all surveys and their quadrats so that we add 0s where appropriate (in case sites with no seagrass get deleted in the summary process)
  select(date,siteID,quadrat)%>%
  mutate(date=mdy(date))%>%
  distinct()

pc3<-pc%>%
  filter(taxa %in% c("T","H","S","He","T "))%>%# keep only seagrass rows
  mutate(taxa=ifelse(taxa=="T ","T",taxa),# some T were entered with a space afterwards, change these to get rid of space 
         percent.cover=100*round((percent.cover/25),2),#calculate percent cover by dividing by 25 (max value) and rounding to 2 decimal places
         date=mdy(date))%>%# make date a date objet
  select(date,siteID,quadrat,taxa,percent.cover)# keep only columns we need going forward

pc4<-left_join(pc2,pc3)%>%#join pc3 onto pc2 so all sites/plots/quadrats are included even if they don't have seagrass
  mutate(percent.cover=ifelse(is.na(percent.cover),0,percent.cover))%>%# turn any NAs into 0s
  pivot_wider(names_from=quadrat,values_from = percent.cover,values_fill=0)%>%# make dataset wider to fill in 0s where records don't exist
  pivot_longer(`1`:`4`,names_to = "quadrat",values_to = "percent.cover")%>%# make dataset long again
  pivot_wider(names_from=taxa,values_from= percent.cover,values_fill=0)%>%# make dataset wider to fill in 0s where records don't exist
  select(-`NA`)%>%#get rid of NA taxa
  pivot_longer(`T`:He,names_to = "taxa",values_to="percent.cover")%>%# make dataset long again
  group_by(date,siteID,taxa)%>%# group so each survey gets a mean percent cover calculated
  summarize(m.pc=mean(percent.cover))#calculate mean percent cover


pc.sum2<-left_join(pc4,bay.ds)%>%# add bays to dataset
  group_by(bay,taxa)%>%# group to summarize by bay and seagrass species
  summarize(m.pcs=round(mean(m.pc),2), # calculate region specific mean percent cover by taxa
            sd.pc=round(sd(m.pc),2))%>%# calculate region specific standard deviation of percent cover by taxa
  mutate(taxa=case_when(# change taxa abbreviations to taxa names
    taxa=="T"~"T. testudinum",
    taxa=="H"~"H. wrightii",
    taxa=="S"~"S. filiforme",
    taxa=="He"~"H. engelmannii"),
    `Percent cover`=paste(m.pcs,"$\\pm$",sd.pc),# add a character column to table that is mean pluminus sd
    Region=case_when(# change region from abbreviations to names
      bay=="NB"~"North Bay",
      bay=="WB"~"West Bay",
      bay=="SJB"~"St. Joseph Bay",
      bay=="SAB"~"St. Andrew Bay"))%>%
  ungroup()%>%
  select(Region,taxa,`Percent cover`)%>% # keep only columns we want to display
  pivot_wider(names_from=taxa,values_from = `Percent cover`) # turn wide

# put regions in order for table
pc.sum2$Region<-factor(pc.sum2$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
pc.sum2<-arrange(pc.sum2,Region)

# put columns in order for table
pc.sum2<-pc.sum2[,c(1,5,2,4,3)]

# shoot density----
shtd2<-shtd%>%
  mutate(shoot.density=shoot.density/.01,# calcualte per m2 shoot densities
         date=mdy(date))# turn date into date object.

sht3<-left_join(pc2,shtd2)%>% # do the same thing we did with % cover to make sure there are no NAs 0
  select(-notes,-QAQC)%>%
  mutate(shoot.density=ifelse(is.na(shoot.density),0,shoot.density))%>%
  pivot_wider(names_from = taxa,values_from = shoot.density,values_fill=0)%>%
  pivot_longer(`T`:`He`,names_to="taxa",values_to = "shoot.density")%>%
  pivot_wider(names_from = quadrat,values_from=shoot.density,values_fill=0)%>%
  pivot_longer(`1`:`4`,names_to="quadrat",values_to = "shoot.density")

sht.sum2<-left_join(sht3,bay.ds)%>%
  group_by(bay,siteID,taxa)%>% # group to calculate survey-specific means
  summarize(m.shts=round(mean(shoot.density),2))%>%# calculate survey-specific mean shoot density
  ungroup()%>%
  group_by(bay,taxa)%>% # group to calcualte region specific means
  summarize(m.sht=round(mean(m.shts),2),# mean shoot density
            sd.sht=round(sd(m.shts),2))%>% # sd shoot density
  mutate(taxa=case_when(# get rid of abbreviations
    taxa=="T"~"T. testudinum",
    taxa=="H"~"H. wrightii",
    taxa=="S"~"S. filiforme",
    taxa=="He"~"H. engelmannii"),
    `Shoot density (shoots per m2)`=paste(m.sht,"$\\pm$",sd.sht),# make character column for table
    Region=case_when(#get rid of abbreviations
      bay=="NB"~"North Bay",
      bay=="WB"~"West Bay",
      bay=="SJB"~"St. Joseph Bay",
      bay=="SAB"~"St. Andrew Bay"))%>%
  ungroup()%>%
  select(Region,taxa,`Shoot density (shoots per m2)`)%>%# keep only columns we want
  pivot_wider(names_from=taxa,values_from = `Shoot density (shoots per m2)`)#make wider

#organize rows and columns to be in order we want
sht.sum2$Region<-factor(sht.sum2$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
sht.sum2<-arrange(sht.sum2,Region)
sht.sum2<-sht.sum2[,c(1,6,2,5,3)]

# combine shoot density and % cover data to make the table
sg.sum<-bind_rows(pc.sum2,sht.sum2)

#make table
sg.table<-kbl(sg.sum,booktabs=T,align="c",
              format = 'latex', escape = FALSE,
              caption="Seagrass percent cover and shoot density estimates by region (mean $\\pm$ sd) for Thalassia testudinum, Halodule wrightii, Syringodium filiforme, and Halophila engelmannii")%>% 
  pack_rows(index=c("Percent cover"=4,"Shoots per square meter"=4),
            indent=TRUE,
            italic=TRUE,
            bold=FALSE)%>%
  column_spec(1,width="1.3in")%>%
  column_spec(2,width="1.2in")%>%
  column_spec(3,width="1.2in")%>%
  column_spec(4,width="1.2in")%>%
  column_spec(5,width="1.2in")


# now get species richness and diversity of seagrasses and macroalgae
#seagrass.spr----
# make dataset wide to use vegan to calculate species richness and diversity
pc5<-pc4%>%
  pivot_wider(names_from = taxa,values_from = m.pc)
#calculate seagrass species richness and diversity
plants<-data.frame(date=pc5$date,siteID=pc5$siteID,
                   seagrass.spr=specnumber(pc5[,-1:-2]),
                   seagrass.div=round(diversity(pc5[,-1:-2]),2))
# get "rid" of seagrass to calculate macroalgae species richness and diversity. 
# have to do all the steps we did for seagrass above to make sure we keep 
#sites/surveys where nothing was seen.
pc6<-pc%>%
  filter(!taxa %in% c("T","H","S","He","T "))%>%
  mutate(percent.cover=100*round((percent.cover/25),2),
         date=mdy(date))%>%
  select(date,siteID,quadrat,taxa,percent.cover)

pc7<-left_join(pc2,pc6)%>%
  pivot_wider(names_from=quadrat,values_from = percent.cover,values_fill=0)%>%
  pivot_longer(`1`:`4`,names_to = "quadrat",values_to = "percent.cover")%>%
  pivot_wider(names_from=taxa,values_from= percent.cover,values_fill=0)%>%
  select(-`NA`)%>%
  pivot_longer(LAU:ACE,names_to = "taxa",values_to="percent.cover")%>%
  group_by(date,siteID,taxa)%>%
  summarize(m.pc=mean(percent.cover))%>%
  pivot_wider(names_from=taxa,values_from= m.pc,values_fill=0)


plants2<-data.frame(date=pc7$date,siteID=pc7$siteID,
                  macroalgae.spr=specnumber(pc7[,-1:-2]),
                  macroalgae.div=round(diversity(pc7[,-1:-2]),2),
                  macroalgae.pc=rowSums(pc7[,-1:-2]))
# make things pretty for table
plant<-left_join(bay.ds,plants)%>%
  left_join(plants2)%>%
  group_by(bay)%>%
  summarize(sgspr=mean(seagrass.spr),
            sgspr.sd=sd(seagrass.spr),
            sgdiv=mean(seagrass.div),
            sgdiv.sd=sd(seagrass.div),
            maspr=mean(macroalgae.spr),
            maspr.sd=sd(macroalgae.spr),
            madiv=mean(macroalgae.div),
            madiv.sd=sd(macroalgae.div),
            mapc=mean(macroalgae.pc),
            mapc.sd=sd(macroalgae.pc))%>%
  mutate(across(2:11,~round(.x,2)),
         Region=case_when(
           bay=="NB"~"North Bay",
           bay=="WB"~"West Bay",
           bay=="SJB"~"St. Joseph Bay",
           bay=="SAB"~"St. Andrew Bay"),
         `Species richness`=paste(sgspr,"$\\pm$",sgspr.sd),
         `Diversity (H)`=paste(sgdiv,"$\\pm$",sgdiv.sd),
         `Taxa richness`=paste(maspr,"$\\pm$",maspr.sd),
         `Taxa Diversity (H)`=paste(madiv,"$\\pm$",madiv.sd),
         `Percent cover`=paste(mapc,"$\\pm$",mapc.sd))%>%
  select(-2:-11)

# put rows and columns in order for table
plant$Region<-factor(plant$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
plant<-arrange(plant,Region)
plant<-plant[,2:7]


#make table
plants.tab<-kbl(plant,booktabs=T,
                format = 'latex', escape = FALSE,
                align="c",
                caption="Seagrass and macroalgae richness and diversity (mean $\\pm$ sd) as well as macroalgae percent cover (mean $\\pm$ sd) by region.")%>%
  add_header_above(c("","Seagrass"=2,"Macroalgae"=3))%>%
  column_spec(1,width="1.25in")%>%
  column_spec(2,width="1in")%>%
  column_spec(3,width="1in")%>%
  column_spec(4,width="1in")%>%
  column_spec(5,width="1in")%>%
  column_spec(6,width="1in")
  
#urchins----

#counts first
uc2<-left_join(urchin_count,bay.ds)%>%
  mutate(date=mdy(date),#turn date into a date object
         mnth=month(date),#extract the month of the survey
         udens=tally/50)%>%# chalculate urchin density- transect was 50m by 1m
  group_by(mnth,bay)%>%
  summarize(m.urch=round(mean(udens),2),#mean urchin density
            max.urch=round(max(udens),2))%>%# maximum urchin density
  distinct()%>%
  mutate(mnth=case_when(
    mnth==5~"May",
    mnth==6~"June",
    mnth==7~"July"),
    u.c=paste(m.urch,"(",max.urch,")"))%>%
  select(-m.urch,-max.urch)%>%
  pivot_wider(names_from = mnth,values_from = c(u.c))%>%
  mutate(July=ifelse(is.na(July),"-",July))

us2<-left_join(urchin_size,bay.ds)%>%
  mutate(date=mdy(date),#turn date into a date object
         mnth=month(date))%>%#extract the month of the survey
  group_by(mnth,bay)%>%
  summarize(min.size=min(size),
            max.size=max(size),
            med.size=median(size))%>%
  distinct()%>%
  mutate(u.s=paste(med.size,"(",min.size,"-",max.size,")"))%>%
  select(-min.size,-max.size,-med.size)%>%
  pivot_wider(names_from = mnth,values_from = u.s)

urch<-left_join(uc2,us2)%>%
  mutate(`5`=ifelse(is.na(`5`),"-",`5`),# put `-` wherever there were no urchins measured
         `6`=ifelse(is.na(`6`),"-",`6`),
         `7`=ifelse(is.na(`7`),"-",`7`),
         bay=case_when(
           bay=="NB"~"North Bay",
           bay=="WB"~"West Bay",
           bay=="SJB"~"St. Joseph Bay",
           bay=="SAB"~"St. Andrew Bay"))%>%
  rename(Region=bay)

# put rows in order for table
urch$Region<-factor(urch$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
urch<-arrange(urch,Region)

# change column name for table
colnames(urch)[5:7]<-c("May","June","July")

#make table
urch.table<-kbl(urch,booktabs=T,align="c",
                format = 'latex', escape = FALSE,
                 caption="Mean Urchin density (maximum) and median size (min-max) recorded in each region over the course of the survey.")%>%
  add_header_above(c("","Urchins per square meter"=3,"Urchin size (cm)"=3))%>%
  column_spec(1,width="1in")%>%
  column_spec(2,width="0.8in")%>%
  column_spec(3,width="0.8in")%>%
  column_spec(4,width="0.8in")%>%
  column_spec(5,width="1.1in")%>%
  column_spec(6,width="1.1in")%>%
  column_spec(7,width="1.1in")

# invertebrate community----
# make dataset wide to take advantage of vegan package
inv2<-inverts%>%
  mutate(date=mdy(date))%>%
  right_join(pc2)%>%# join to pc2 so that surveys and quadrats that have no inverts are give a value of 0
  left_join(bay.ds)%>%
  group_by(date,bay,siteID,taxa)%>%
  summarize(abundance=sum(abundance))%>%# add abundance together for each survey
  distinct()%>%# keep only distinct rows
  pivot_wider(names_from = taxa,values_from=abundance,values_fill=0)%>%#make dataset wide
  select(-`NA`)# remove the NA column

inv3<-data.frame(Region=inv2$bay,
                 spr=specnumber(inv2[,-1:-3]),
                 div=diversity(inv2[,-1:-3]))%>%
  group_by(Region)%>%
  summarize(m.spr=round(mean(spr),2),
            sd.sp=round(sd(spr),2),
            m.div=round(mean(div),2),
            sd.div=round(sd(div),2))%>%
  distinct()%>%
  mutate(`Taxa richness`=paste(m.spr, "$\\pm$", sd.sp),
         `Taxa diversity (H)`=paste(m.div, "$\\pm$",sd.div),
         Region=case_when(
           Region=="NB"~"North Bay",
           Region=="WB"~"West Bay",
           Region=="SJB"~"St. Joseph Bay",
           Region=="SAB"~"St. Andrew Bay"))%>%
  select(Region,`Taxa richness`,`Taxa diversity (H)`)

# put rows in order for table
inv3$Region<-factor(inv3$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
inv3<-arrange(inv3,Region)

# make table
inv.table<-kbl(inv3,booktabs = T,align="c",
               format = 'latex', escape = FALSE,
               caption="Sessile and slow-moving invertebrate taxa richness and diversity.")%>%
  column_spec(1,width="1.15in")%>%
  column_spec(2,width = "1.3in")%>%
  column_spec(3,width="1.3in")

#lab seagrass biomass---
# calculate total, average, and stdev of biomass
# for each seagrass Tt in each region
bio.sum<-biomass%>%# start organizing for table
  filter(sample.type!="epiphytes")%>%#remove epiphytes from biomass estimates
  group_by(bay,siteID)%>% # group to calculate survey-specific values
  summarize(sg.bio=sum(sample.weight,na.rm=T))%>%
  distinct()%>%
  ungroup()%>%
  group_by(bay)%>%
  #filter(species=="Tt")%>% #select Tt data only
  summarize(m.site.bio=round(mean(sg.bio,na.rm = T),2),# average biomass at each site
         sd.site.bio=round(sd(sg.bio,na.rm = T),2),#standard deviation biomass at each site
         sum.site.bio=round(sum(sg.bio,na.rm = T),2))%>%#total biomass per bay/region
  mutate(Region=case_when(# turn region abbreviations into nice column labels for table
        bay=="NB"~"North Bay",
        bay=="WB"~"West Bay",
        bay=="SJB"~"St. Joseph Bay",
        bay=="SAB"~"St. Andrew Bay"),
        sd.site.bio=ifelse(is.na(sd.site.bio),0,sd.site.bio),
        `Mean biomass (g)`=paste(m.site.bio,"$\\pm$",sd.site.bio),
        `Total above-ground biomass removed (g)`=sum.site.bio)%>%
  select(Region,`Mean biomass (g)`,`Total above-ground biomass removed (g)`)

# set order of regions for table
bio.sum$Region<-factor(bio.sum$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay","Total"))
bio.sum<-arrange(bio.sum,Region)

# add total row
bio.total<-data.frame(v1="Total",v2="",v3=sum(bio.sum[,3]))
colnames(bio.total)<-colnames(bio.sum)
bio.sum<-bind_rows(bio.sum,bio.total)
# change column name for table
# colnames(bio.sum)<-c("Region","Mean per site", "Standard Deviation", "Total")

# #repeat for Hw
# bio.sum2<-biomass%>%# start organizing for table
#   group_by(bay)%>% # group to calculate survey-specific values
#   filter(species=="Hw")%>% #select Hw data only
#   summarize(m.site.bio=round(mean(sample.weight,na.rm = T),2),# average biomass at each site
#             sd.site.bio=round(sd(sample.weight,na.rm = T),2),#standard deviation biomass at each site
#             sum.site.bio=round(sum(sample.weight,na.rm = T),2))%>%#total biomass per bay/region
#   mutate(bay=case_when(# turn region abbreviations into nice column labels for table
#         bay=="NB"~"North Bay",
#         bay=="WB"~"West Bay",
#         bay=="SJB"~"St. Joseph Bay",
#         bay=="SAB"~"St. Andrew Bay"))

#bio.sum3 is not working as intended, I was hoping to have redundant mean, sd, and total columns for Tt and Hw
#I decided to report Hw in the text rather than making a larger table with a lot of 0s, 
#but in the future we may want to expand the table to include additional species
# bio.sum3<-left_join(bio.sum,bio.sum2)%>%#join results from bio.sum and bio.sum2 to group under Tt and Hw, respectively, in same table
#   mutate(m.site.bio=ifelse(is.na(m.site.bio),0,m.site.bio))# turn any NAs into 0s
# 


# make table 
bio.table<-kbl(bio.sum,booktabs = T,align="c",
               format = 'latex', escape = FALSE,
               caption="Above-ground biomass (g of dry weight, mean $\\pm$ sd) of seagrass by region as well as the total amount of seagrass biomass removed for each region.")%>%
  column_spec(1,width="1.15in")%>%
  column_spec(2,width = "1.3in")%>%
  column_spec(3,width="1.3in")%>%
  row_spec(4,hline_after=TRUE)

#lab seagrass growth---
gdates<-biomass%>%
  select(siteID,bay,mark.date,collect.date)%>%
  distinct()

grow2<-growth%>%
  filter(!is.na(new.length))%>%
  distinct()%>%
  left_join(gdates)%>%
  mutate(mark.date=mdy(mark.date),
         collect.date=mdy(collect.date),
         days.grow=as.numeric(collect.date-mark.date),
         elong=new.length/days.grow)%>%
  group_by(siteID,bay)%>%
  mutate(site.elong=sum(new.length)/days.grow)%>%
  ungroup()%>%
  group_by(bay)%>%
  mutate(mleaf=round(mean(elong),2),
            sdleaf=round(sd(elong),2))%>%
  select(bay,mleaf,sdleaf,site.elong)%>%
  distinct()%>%
  mutate(msite=round(mean(site.elong),2),
            sdsite=ifelse(is.na(sd(site.elong)),0,round(sd(site.elong),2)))%>%
  select(Region=bay,mleaf,sdleaf,msite,sdsite)%>%
  distinct()%>%
  mutate(`Leaf elongation rate`=paste(mleaf,"$\\pm$",sdleaf),
         `Site-level elongation rate`=paste(msite,"$\\pm$",sdsite),
         Region=case_when(# turn region abbreviations into nice column labels for table
           Region=="NB"~"North Bay",
           Region=="WB"~"West Bay",
           Region=="SJB"~"St. Joseph Bay",
           Region=="SAB"~"St. Andrew Bay"))%>%
  select(Region,`Leaf elongation rate`,`Site-level elongation rate`)

# set order of regions for table
grow2$Region<-factor(grow2$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay","Total"))
grow2<-arrange(grow2,Region)        

# make table
grow.table<-kbl(grow2,booktabs = T,align="c",
                format = 'latex', escape = FALSE,
                caption="Seagrass growth rates (elongation: cm/day) per leaf and total per site within each region.")%>%
  column_spec(1,width="1.15in")%>%
  column_spec(2,width = "1.3in")%>%
  column_spec(3,width="1.3in")


#example to push to git