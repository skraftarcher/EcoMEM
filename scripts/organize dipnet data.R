# organize dipnet data

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")


lp("tidyverse")
lp("readxl")
library(obistools)
lp("worrms")

# load data
bdiv<-read_xlsx(paste0("odata/EXPERIMENT - LAB - Dipnet",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=2)
tax<-read_xlsx(paste0("odata/EXPERIMENT - LAB - Dipnet",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=4)
pos<-read_xlsx(paste0("odata/EXPERIMENT - LAB - Dipnet",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=2)%>%
  group_by(plotID)
samplings<-read_xlsx(paste0("odata/EXPERIMENT - Sampling Dates",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=1)

wIDs<-distinct(tax[,2:3])%>%
  filter(!is.na(scientific))%>%
  filter(UpdateTaxaID!="animal-0")
# match with worms database

wormsIDs<-data.frame(wIDs,match_taxa(wIDs$scientific))

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
  #filter(!is.na(dry.weight))%>%
  mutate(Dry.weight.g=dry.weight-tin.weight,
         Dry.weight.g=ifelse(Dry.weight.g<0,0.001,Dry.weight.g))%>%
  select(plotID,sample.date,taxaID,abundance,Dry.weight.g)%>%
  left_join(wID2)%>%
  left_join(pos)

bdiv2<-bdiv2[-grep(bdiv2$UpdateTaxaID,pattern="egg"),]
bdiv2<-bdiv2[-grep(bdiv2$UpdateTaxaID,pattern="WTF"),]

bdiv3<-bdiv2[,c(1:2,6:7,10:11,4:5)]%>%
  rename(date=sample.date)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]),
    time.var=case_when(
      sampling=="s1"~1,
      sampling=="s2"~2,
      sampling=="s3"~3,
      sampling=="s4"~4,
      sampling=="s5"~5,
    ))
  
# write.csv(bdiv2,"wdata/ECOMEM_dipnet_Sept232025_novouchersamples.csv",row.names=F)
write.csv(bdiv3,"wdata/feb32026_dipnet.csv",row.names = F)
