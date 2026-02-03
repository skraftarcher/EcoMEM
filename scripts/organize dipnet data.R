# organize dipnet data

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2


lp("tidyverse")
lp("readxl")
library(obistools)
lp("worrms")

# load data
bdiv<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - LAB - Dipnet.xlsx"),sheet=2)
tax<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - LAB - Dipnet.xlsx"),sheet=4)
pos<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Plot Coordinates.xlsx"),sheet=2)%>%
  group_by(plotID)%>%
  summarize(Latitude=mean(Lat),
            Longitude=mean(Long))

wIDs<-distinct(tax[,2:3])%>%
  filter(!is.na(scientific))%>%
  filter(UpdateTaxaID!="animal-0")
# match with worms database

wormsIDs<-data.frame(wIDs,match_taxa(wIDs$scientific))

y

# pull out spp records and add spp to worms scientific names
wormspp<-wormsIDs[grep(wormsIDs$scientific,pattern=" sp"),]%>%
  separate(scientific,into=c("genus","sp"),sep=" ",remove=F)%>%
  separate(sp,into=c("c1","c2"),sep=-1,remove=F)%>%
  mutate(c2=as.numeric(c2))%>%
  filter(!is.na(c2))%>%
  mutate(scientificName=paste(scientificName,sp))%>%
  select(-genus,-sp,-c1,-c2)

wormscf<-wormsIDs[grep(wormsIDs$scientific,pattern=" cf "),]%>%
  mutate(scientificName=scientific)
wormsIDs2<-filter(wormsIDs,!UpdateTaxaID %in% wormspp$UpdateTaxaID)%>%
  bind_rows(wormspp)%>%
  filter(!UpdateTaxaID %in% wormscf$UpdateTaxaID)%>%
  bind_rows(wormscf)%>%
  select(-scientific,-match_type)

wID2<-left_join(distinct(tax[,1:2]),wormsIDs2)


bdiv2<-bdiv%>%
  filter(!is.na(dry.weight))%>%
  mutate(Dry.weight.g=dry.weight-tin.weight,
         Dry.weight.g=ifelse(Dry.weight.g<0,0.001,Dry.weight.g))%>%
  select(plotID,sample.date,taxaID,abundance,Dry.weight.g)%>%
  left_join(wID2)%>%
  left_join(pos)

bdiv2<-bdiv2[-grep(bdiv2$UpdateTaxaID,pattern="egg"),]
bdiv2<-bdiv2[-grep(bdiv2$UpdateTaxaID,pattern="WTF"),]

bdiv2<-bdiv2[,c(1,10,11,2,7,8,9,4,5)]
write.csv(bdiv2,"wdata/ECOMEM_dipnet_Sept232025_novouchersamples.csv",row.names=F)
