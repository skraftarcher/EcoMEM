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

# organize/summarize data by bay
#propeller scars----
prop.sum<-propscar%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE)

prop.sum.oth<-filter(prop.sum,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))

prop.sum2<-prop.sum%>%
  filter(!is.na(num))%>%
  bind_rows(prop.sum.oth)%>%
  mutate(date=mdy(date),
         mnth=month(date),
         mnth=ifelse(mnth==5,5,6),
         length.20=ifelse(is.na(as.numeric(length.shore))|is.na(as.numeric(length.water)),"yes","no"),
         length1=parse_number(length.shore),
         length2=parse_number(length.water),
         length=ifelse(length.20=="no",length1+length2,length1+length2+1),
         rhizomes=ifelse(cut.rizhomes=="Y",1,0),
         regrow=ifelse(seagrass.regrow=="Y",1,0))%>%
  group_by(mnth,siteID,bay)%>%
  mutate(total.length=sum(length))%>%
  ungroup()%>%
  group_by(mnth,bay)%>%
  mutate(n.sites=length(unique(siteID)))%>%
  ungroup()%>%
  group_by(mnth,bay,n.sites)%>%
  filter(length!=0)%>%
  summarize(n.withprops=length(unique(siteID)),
            prop.withscars=n.withprops/n.sites,
            n.prop.scars=n(),
            mean.prop.scars=n.prop.scars/n.sites,
            ave.length=mean(length),
            avg.width=mean(width,na.rm = T),
            avg.depth=mean(depth,na.rm = T),
            min.length=min(length),
            max.length=max(length),
            prop.scars.with.cut.rhizomes=sum(rhizomes)/n.prop.scars,
            prop.scars.with.seagrass.regrowth=sum(regrow)/n.prop.scars)%>%
  distinct()%>%
  mutate(max.length=ifelse(max.length>20,">20",as.character(max.length)))

# make table

# only June/July sites first then we can look at sites that were revisited and how estimates changed
#make pretty
prop.sum3<-filter(prop.sum2,mnth==6)%>%
  ungroup()%>%
  select(Region=bay,
         `Number of sites surveyed`=n.sites,
         `Proportion of sites with scars`=prop.withscars,
         `Mean number of scars per site`=mean.prop.scars,
         `Minimum scar length (m)`=min.length,
         `Maximum scar length (m)`=max.length,
         `Average scar width (m)`=avg.width,
         `Average scar depth (m)`=avg.depth,
         `Proportion of scars with cut rhizomes`=prop.scars.with.cut.rhizomes,
         `Proportion of scars with evidence of regrowth`=prop.scars.with.seagrass.regrowth)%>%
  mutate(across(c(3:5,7:10),~round(.x,2)),
         Region=case_when(
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
  #kable_styling(full_width=T)%>%
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

# now organize to look at sites that were resurveyed.

prop.sum4<-prop.sum%>%
  filter(!is.na(num))%>%
  bind_rows(prop.sum.oth)%>%
  mutate(date=mdy(date),
         mnth=month(date),
         length.20=ifelse(is.na(as.numeric(length.shore))|is.na(as.numeric(length.water)),"yes","no"),
         length1=parse_number(length.shore),
         length2=parse_number(length.water),
         length=ifelse(length.20=="no",length1+length2,length1+length2+1),
         rhizomes=ifelse(cut.rizhomes=="Y",1,0),
         regrow=ifelse(seagrass.regrow=="Y",1,0))%>%
  group_by(mnth,siteID,bay)%>%
  summarize(total.length=sum(length),
         n.prop.scars=ifelse(total.length==0,0,n()),
         ave.length=mean(length),
         avg.width=mean(width,na.rm = T),
         avg.depth=mean(depth,na.rm = T),
         min.length=min(length),
         max.length=max(length),
         prop.scars.with.cut.rhizomes=sum(rhizomes)/n.prop.scars,
         prop.scars.with.seagrass.regrowth=sum(regrow)/n.prop.scars,
         prop.scars.with.cut.rhizomes=ifelse(is.na(prop.scars.with.cut.rhizomes),0,prop.scars.with.cut.rhizomes),
         prop.scars.with.seagrass.regrowth=ifelse(is.na(prop.scars.with.seagrass.regrowth),0,prop.scars.with.seagrass.regrowth))%>%
  distinct()
  
prop.sum.may<-filter(prop.sum4,mnth==5)%>%
  ungroup()%>%
  select(-mnth)%>%
  pivot_longer(3:11,names_to="vars",values_to="May")%>%
  mutate(May=round(May,2))

prop.sum.june<-filter(prop.sum4,mnth==6)%>%
  filter(siteID %in% unique(prop.sum.may$siteID))%>%
  ungroup()%>%
  select(-mnth)%>%
  pivot_longer(3:11,names_to="vars",values_to="June")%>%
  mutate(June=round(June,2))

prop.sum.july<-filter(prop.sum4,mnth==7)%>%
  filter(siteID %in% unique(prop.sum.may$siteID))%>%
  ungroup()%>%
  select(-mnth)%>%
  pivot_longer(3:11,names_to="vars",values_to="July")%>%
  mutate(July=round(July,2))

prop.comp<-left_join(prop.sum.may,prop.sum.june)%>%
  left_join(prop.sum.july)%>%
  mutate(samplings=case_when(
    is.na(June) & is.na(July)~1,
    is.na(June) & !is.na(July)~2,
    !is.na(June) & is.na(July)~2,
    !is.na(June) & !is.na(July)~3),
    samp2=case_when(
      samplings==2 & !is.na(June) ~ June,
      samplings==2 & is.na(June) ~ July),
    mjj=(May+June+July)/3,
    diff=case_when(
      samplings==1~-99,
      samplings==2~May-samp2,
      samplings==3 & mjj==0~0,
      samplings==3 & mjj!=0 ~ sqrt(((May-mjj)^2+(June-mjj)^2+(July-mjj)^2)/3)
  ),
  diff=round(diff,2))%>%
  select(siteID,bay,vars,samplings,diff)%>%
  filter(diff!=-99)%>%
  pivot_wider(names_from=vars,values_from=diff)

# overall we saw more prop scars in May... not sure what to make about that.

# calculate average canopy height, percent cover, & shoot density 
# for seagrasses in each region for each month
#canopy height----
can.sum<-canopy%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE)%>%
  filter(date!="")

can.sum.oth<-filter(can.sum,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))

can.sum2<-filter(can.sum,!is.na(num))%>%
  bind_rows(can.sum.oth)%>%
  pivot_longer(canopy1:canopy4,names_to = "meas",values_to="can.ht")%>%
  mutate(date=mdy(date),
         grazing=ifelse(grazing=="n",0,1))%>%
  group_by(date,siteID)

# I'm not confident that all quadrats had canopy heights recorded
# pulling out date, siteID, and quadrat from percent cover data to make sure
can.check<-can.sum2%>%
  select(date,siteID,quadrat)%>%
  mutate(has.canopy=1)%>%
  distinct()

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

can.sum3<-can.sum2%>%
  mutate(mnth=month(date))%>%
  group_by(date,bay,mnth,siteID)%>%
  mutate(m.site.ht=mean(can.ht,na.rm = T),
            site.grazed=ifelse(sum(grazing,na.rm = T)==0,0,1))%>%
  ungroup()%>%
  group_by(mnth,bay)%>%
  summarize(mean.ht=round(mean(m.site.ht),2),
            min.ht=min(can.ht,na.rm=T),
            max.ht=max(can.ht,na.rm=T),
            proportion.grazed=round(sum(site.grazed)/n(),2))%>%
  mutate(mnth=case_when(
    mnth==5~"May",
    mnth==6~"June",
    mnth==7~"July"),
    bay=case_when(
      bay=="NB"~"North Bay",
      bay=="WB"~"West Bay",
      bay=="SJB"~"St. Joseph Bay",
      bay=="SAB"~"St. Andrew Bay"),
    ht.range=paste(mean.ht,"(",min.ht,"-",max.ht,")"))%>%
  select(-min.ht,-max.ht,-mean.ht)%>%
  pivot_wider(names_from=mnth,values_from = c(ht.range,proportion.grazed))

can.sum3$Region<-factor(can.sum3$bay,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
can.sum3<-arrange(can.sum3,Region)

can.sum4<-can.sum3[,c(8,2:7)]%>%
  mutate(ht.range_July=ifelse(is.na(ht.range_July),"-",ht.range_July),
         proportion.grazed_July=ifelse(is.na(proportion.grazed_July),"-",proportion.grazed_July))

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
pc2<-pc%>%
  select(date,siteID,quadrat)%>%
  mutate(date=mdy(date))%>%
  distinct()

pc3<-pc%>%
  filter(taxa %in% c("T","H","S","He","T "))%>%
  mutate(taxa=ifelse(taxa=="T ","T",taxa),
         percent.cover=100*round((percent.cover/25),2),
         date=mdy(date))%>%
  select(date,siteID,quadrat,taxa,percent.cover)

pc4<-left_join(pc2,pc3)%>%
  pivot_wider(names_from=quadrat,values_from = percent.cover,values_fill=0)%>%
  pivot_longer(`1`:`4`,names_to = "quadrat",values_to = "percent.cover")%>%
  pivot_wider(names_from=taxa,values_from= percent.cover,values_fill=0)%>%
  select(-`NA`)%>%
  pivot_longer(`T`:He,names_to = "taxa",values_to="percent.cover")%>%
  group_by(date,siteID,taxa)%>%
  summarize(m.pc=mean(percent.cover))


pc.sum<-pc4%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE)

pc.sum.oth<-filter(pc.sum,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))

pc.sum2<-pc.sum%>%
  filter(!is.na(num))%>%
  bind_rows(pc.sum.oth)%>%
  group_by(bay,taxa)%>%
  summarize(m.pcs=round(mean(m.pc,na.rm=T),2),
            sd.pc=round(sd(m.pc,na.rm=T),2))%>%
  mutate(taxa=case_when(
    taxa=="T"~"T. testudinum",
    taxa=="H"~"H. wrightii",
    taxa=="S"~"S. filiforme",
    taxa=="He"~"H. engelmannii"),
    `Percent cover`=paste(m.pcs,"$\\pm$",sd.pc),
    Region=case_when(
      bay=="NB"~"North Bay",
      bay=="WB"~"West Bay",
      bay=="SJB"~"St. Joseph Bay",
      bay=="SAB"~"St. Andrew Bay"))%>%
  ungroup()%>%
  select(Region,taxa,`Percent cover`)%>%
  pivot_wider(names_from=taxa,values_from = `Percent cover`)

pc.sum2$Region<-factor(pc.sum2$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
pc.sum2<-arrange(pc.sum2,Region)

pc.sum2<-pc.sum2[,c(1,5,2,4,3)]

# shoot density----
shtd2<-shtd%>%
  mutate(shoot.density=shoot.density/.01,
         date=mdy(date))

sht3<-left_join(pc2,shtd2)%>%
  select(-notes,-QAQC)%>%
  pivot_wider(names_from = taxa,values_from = shoot.density,values_fill=0)%>%
  pivot_longer(`T`:`He`,names_to="taxa",values_to = "shoot.density")%>%
  pivot_wider(names_from = quadrat,values_from=shoot.density,values_fill=0)%>%
  pivot_longer(`1`:`4`,names_to="quadrat",values_to = "shoot.density")

sht.sum<-sht3%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE)

sht.sum.oth<-filter(sht.sum,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))
  
sht.sum2<-sht.sum%>%
  filter(!is.na(num))%>%
  bind_rows(sht.sum.oth)%>%
  group_by(bay,siteID,taxa)%>%
  summarize(m.shts=round(mean(shoot.density,na.rm=T),2))%>%
  ungroup()%>%
  group_by(bay,taxa)%>%
  summarize(m.sht=round(mean(m.shts,na.rm=T),2),
            sd.sht=round(sd(m.shts,na.rm=T),2))%>%
  mutate(taxa=case_when(
    taxa=="T"~"T. testudinum",
    taxa=="H"~"H. wrightii",
    taxa=="S"~"S. filiforme",
    taxa=="He"~"H. engelmannii"),
    `Shoot density (shoots per m2)`=paste(m.sht,"$\\pm$",sd.sht),
    Region=case_when(
      bay=="NB"~"North Bay",
      bay=="WB"~"West Bay",
      bay=="SJB"~"St. Joseph Bay",
      bay=="SAB"~"St. Andrew Bay"))%>%
  ungroup()%>%
  select(Region,taxa,`Shoot density (shoots per m2)`)%>%
  pivot_wider(names_from=taxa,values_from = `Shoot density (shoots per m2)`)

sht.sum2$Region<-factor(sht.sum2$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
sht.sum2<-arrange(sht.sum2,Region)
sht.sum2<-sht.sum2[,c(1,6,2,5,3)]

sg.sum<-bind_rows(pc.sum2,sht.sum2)


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

pc5<-pc4%>%
  pivot_wider(names_from = taxa,values_from = m.pc)

plants<-data.frame(date=pc5$date,siteID=pc5$siteID,
                   seagrass.spr=specnumber(pc5[,-1:-2]),
                   seagrass.div=round(diversity(pc5[,-1:-2]),2))

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

bays<-pc.sum%>%
  filter(!is.na(num))%>%
  bind_rows(pc.sum.oth)%>%
  select(date,siteID,bay)%>%
  distinct()

plant<-left_join(bays,plants)%>%
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

plant$Region<-factor(plant$Region,levels=c("St. Joseph Bay","St. Andrew Bay","North Bay","West Bay"))
plant<-arrange(plant,Region)
plant<-plant[,2:7]

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
  
  
