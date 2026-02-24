# organize percent cover, shoot density, and canopy height data 

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")

lp("tidyverse")
lp("readxl")

# load data----
pc<-read_xlsx(paste0("odata/EXPERIMENT - Seagrass and macroalgae Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=2)
sd<-read_xlsx(paste0("odata/EXPERIMENT - Seagrass and macroalgae Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=3)
can<-read_xlsx(paste0("odata/EXPERIMENT - Seagrass and macroalgae Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=4)
samplings<-read_xlsx(paste0("odata/EXPERIMENT - Sampling Dates",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=1)

# organize pc data ----
pc.check<-pc%>%
  filter(quadrat<=4)%>%
  group_by(date,plotID,quadrat,taxa)%>%
  summarize(ns=n())%>%
  filter(ns>1)

pc2<-pc%>%
  filter(quadrat<=4)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    taxa=case_when(
      taxa=="GRA"~"GRAC",
      taxa=="Grac"~"GRAC",
      taxa=="OB58"~"OB1",
      TRUE~taxa),
    quadrat=paste0("q",quadrat),
    percent.cover=(percent.cover/25)*100)%>%
  left_join(samplings[,3:4])%>%
  select(-Notes,-QAQC,-surveyor)%>%
  pivot_wider(names_from = taxa,values_from=percent.cover,values_fill=0)%>%
  pivot_longer(-1:-6,names_to="taxa",values_to="pcover")%>%
  pivot_wider(names_from=quadrat,values_from=pcover,values_fill = 0)%>%
  pivot_longer(q1:q4,names_to="quadrat",values_to="pcover")%>%
  group_by(date,blockID,plotID,sampling,mnths,taxa)%>%
  mutate(m.cover=mean(pcover))%>%
  pivot_wider(names_from=quadrat,values_from=pcover)%>%
  ungroup()%>%
  select(sampling,mnths,blockID,plotID,taxa,
         q1,q2,q3,q4,m.cover)

write.csv(pc2,"wdata/experiment percent cover quadrat wide.csv",row.names = FALSE)

# organize shoot density ----
sd2<-sd%>%
  filter(quadrat<=4)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    quadrat=paste0("q",quadrat))%>%
  left_join(samplings[,3:4])%>%
  select(-notes,-QAQC,-surveyor)%>%
  pivot_wider(names_from = taxa,values_from=shoot.density,values_fill=0)%>% 
  pivot_longer(-1:-6,names_to="taxa",values_to="sdens")%>%
  pivot_wider(names_from=quadrat,values_from=sdens,values_fill = 0)%>%
  pivot_longer(q1:q4,names_to="quadrat",values_to="sdens")%>%
  group_by(date,blockID,plotID,sampling,mnths,taxa)%>%
  mutate(m.density=mean(sdens))%>%
  pivot_wider(names_from=quadrat,values_from=sdens)%>%
  ungroup()%>%
  select(sampling,mnths,blockID,plotID,taxa,
         q1,q2,q3,q4,m.density)

write.csv(sd2,"wdata/experiment shoot density quadrat wide.csv",row.names = FALSE)

# organize canopy data----
can2<-can%>%
  filter(quadrat<=4)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    quadrat=paste0("q",quadrat),
    m.canopy=(canopy1+canopy2+canopy3+canopy4)/4)%>%
  select(-canopy1,-canopy2,-canopy3,-canopy4,-surveyor,-notes,-QAQC,-grazing)%>%
  pivot_wider(names_from = quadrat,values_from = m.canopy)%>%
  mutate(m.canopy=(q1+q2+q3+q4)/4)%>%
  left_join(samplings[,3:4])%>%
  select(sampling,mnths,blockID,plotID,
         q1,q2,q3,q4,m.canopy)

write.csv(sd2,"wdata/experiment canopy quadrat wide.csv",row.names = FALSE)

# make one big dataset of just seagrass with quadrat as a column
pc3<-pc2%>%
  select(-m.cover)%>%
  pivot_longer(q1:q4,names_to = "quadrat",values_to="pcover")%>%
  filter(taxa %in% c("T","H","S"))

sd3<-sd2%>%
  select(-m.density)%>%
  pivot_longer(q1:q4,names_to = "quadrat",values_to="sdensity")

sg<-full_join(pc3,sd3)

write.csv(sg,"wdata/p cover and shoot density by quadrat long.csv",row.names=F)

# make an only thalassia dataset

can3<-can2%>%
  select(-m.canopy)%>%
  pivot_longer(q1:q4,names_to = "quadrat",values_to="canopy")

sg2<-sg%>%
  filter(taxa=="T")%>%
  full_join(can3)

write.csv(sg2,"wdata/thalassia p cover shoot density and canopy by quadrat long.csv",row.names=F)
