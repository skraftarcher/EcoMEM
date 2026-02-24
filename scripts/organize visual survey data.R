# organize visual survey data
# organize dipnet data

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")

lp("tidyverse")
lp("readxl")
lp("worrms")
lp("vegan")
library(obistools)

# load data----
bdiv<-read_xlsx(paste0("odata/EXPERIMENT - Visual Survey Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=2)
tax<-read_xlsx(paste0("odata/EXPERIMENT - Visual Survey Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=4)
samplings<-read_xlsx(paste0("odata/EXPERIMENT - Sampling Dates",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=1)
size<-read_xlsx(paste0("odata/EXPERIMENT - Visual Survey Data",format(Sys.time(), '_%d_%B_%Y'),".xlsx"),sheet=3)%>%
  rename(date=Date,taxaID=taxa)%>%
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

wIDs<-tax%>%
  filter(taxaID %in% unique(bdiv$taxa))%>%
  select(UpdateTaxaID,scientific)%>%
  distinct()%>%
  filter(!is.na(scientific))%>%
  filter(UpdateTaxaID!="animal-0")

wIDs<-wIDs[-grep(wIDs$UpdateTaxaID,pattern="egg"),]

# match with worms database

wormsIDs<-data.frame(wIDs,match_taxa(wIDs$scientific))

wormspp<-wormsIDs[grep(wormsIDs$scientific,pattern=" sp"),]%>%
  separate(scientific,into=c("genus","sp"),sep=" ",remove=F)%>%
  separate(sp,into=c("c1","c2"),sep=-1,remove=F)%>%
  mutate(c2=as.numeric(c2))%>%
  filter(!is.na(c2))%>%
  mutate(scientificName=paste(scientificName,sp))%>%
  select(-genus,-sp,-c1,-c2)
wormscf<-wormsIDs[grep(wormsIDs$scientific,pattern=" cf "),]%>%
  mutate(scientificName=scientific)
wormsna<-filter(wormsIDs,is.na(scientificName))%>%
  mutate(scientificName=scientific)


wormsIDs3<-filter(wormsIDs,!UpdateTaxaID %in% unique(wormspp$UpdateTaxaID))%>%
  bind_rows(wormspp)%>%
  filter(!UpdateTaxaID %in% unique(wormscf$UpdateTaxaID))%>%
  bind_rows(wormscf)%>%
  filter(!UpdateTaxaID %in% unique(wormsna$UpdateTaxaID))%>%
  bind_rows(wormsna)%>%
  select(-scientific,-match_type)%>%
  left_join(tax[,1:2])


wormsIDs_updatetax<-wormsIDs3%>%
  separate(scientificNameID,into=c("other","aphiaID"),sep="taxname:")%>%
  filter(aphiaID!=acceptedNameUsageID)%>%
  select(taxaID,UpdateTaxaID,acceptedNameUsageID)%>%
  mutate(acceptedNameUsageID=as.numeric(acceptedNameUsageID))

scientific<-wm_id2name(wormsIDs_updatetax$acceptedNameUsageID[1])
for(i in 2:nrow(wormsIDs_updatetax))scientific<-c(scientific,wm_id2name(wormsIDs_updatetax$acceptedNameUsageID[i]))

wormsIDs_updatetax$scientific<-scientific 

wormsIDs4<-data.frame(wormsIDs_updatetax[,-3],match_taxa(wormsIDs_updatetax$scientific))


wormsIDs<-wormsIDs3%>%
  filter(!taxaID %in% wormsIDs4$taxaID)%>%
  bind_rows(wormsIDs4)%>%
  select(-acceptedNameUsageID,-scientific,-match_type)%>%
  distinct()

# add actual scientific names to dataset
bdiv2<-bdiv%>%
  select(-scientific.name,-common.name,-Notes,-QAQC)%>%
  rename(taxaID=taxa)%>%
  left_join(wormsIDs)%>%
  select(-scientificNameID)%>%
  filter(!is.na(blockID))%>%
  mutate(UpdateTaxaID=ifelse(is.na(UpdateTaxaID)&taxaID=="animal-0","animal-0",UpdateTaxaID),
         scientificName=ifelse(UpdateTaxaID=="animal-0","animal-0",scientificName),
         scientificName=ifelse(is.na(UpdateTaxaID),"eggs",scientificName))%>%
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
    ))%>%
  group_by(sampling,blockID,plotID,UpdateTaxaID,scientificName,Surveyor)%>%
  summarize(total.n=sum(total.n))

# organize size data - find minimum sizes for all measurements and see what we get rid of
size2<-size%>%
  left_join(wormsIDs)%>%
  select(sampling,blockID,plotID,UpdateTaxaID,scientificName,length.pw,width.cl,height.tel,radius)

size.mins<-size2%>%
  filter(sampling %in% c("s1","S2"))%>%
  group_by(scientificName)%>%
  summarize(length.min=min(length.pw,na.rm=T),
            width.min=min(width.cl,na.rm=T),
            height.min=min(height.tel,na.rm=T),
            radius.min=min(radius,na.rm=T))%>%
  mutate(length.min=ifelse(length.min>=2,2,length.min),
         length.min=ifelse(width.min>= 2,2,width.min),
         height.min=ifelse(height.min>=2,2,height.min),
         radius.min=ifelse(radius.min>=2,2,radius.min))

size.mins[sapply(size.mins, is.infinite)] <- NA

size.mins2<-filter(size2,!scientificName %in% size.mins$scientificName)%>%
  bind_rows(filter(size.mins,is.na(length.min)&is.na(width.min)&is.na(height.min)&is.na(radius.min)))%>%
  select(scientificName)%>%
  distinct()

size.mins2$length.min<-min(size.mins$length.min,na.rm=T)
size.mins2$width.min<-min(size.mins$width.min,na.rm=T)
size.mins2$height.min<-min(size.mins$height.min,na.rm=T)
size.mins2$radius.min<-min(size.mins$radius.min,na.rm=T)


size.mins<-filter(size.mins,!scientificName %in% size.mins2$scientificName)%>%
  bind_rows(size.mins2)


size3<-left_join(size2,size.mins)%>%
  mutate(too.small=case_when(
    length.pw>=length.min|
      width.cl>=width.min|
      height.tel>=height.min|
      radius>=radius.min~"no",
    is.na(length.pw)&
      width.cl>=width.min|
      height.tel>=height.min|
      radius>=radius.min~"no",
    is.na(width.cl)&
      length.pw>=length.min|
      height.tel>=height.min|
      radius>=radius.min~"no",
    length.pw>=length.min|
      width.cl>=width.min|
      radius>=radius.min&
      is.na(height.tel)~"no",
    length.pw>=length.min|
      width.cl>=width.min|
      height.tel>=height.min&
      is.na(radius)~"no",
    is.na(length.pw)&
      is.na(width.cl)&
      height.tel>=height.min|
      radius>=radius.min~"no",
    is.na(length.pw)&
      is.na(height.tel)&
      width.cl>=width.min|
      radius>=radius.min~"no",
    is.na(length.pw)&
      width.cl>=width.min|
      height.tel>=height.min&
      is.na(radius)~"no",
    length.pw>=length.min|
      radius>=radius.min&
      is.na(width.cl)&
      is.na(height.tel)~"no",
    length.pw>=length.min|
      height.tel>=height.min&
      is.na(width.cl)&
      is.na(radius)~"no",
    length.pw>=length.min|
      width.cl>=width.min&
      is.na(height.tel)&
      is.na(radius)~"no",
    is.na(length.pw)&
      is.na(width.cl)&
      is.na(height.tel)&
      radius>=radius.min~"no",
    is.na(length.pw)&
      is.na(width.cl)&
      height.tel>=height.min&
      is.na(radius)~"no",
    length.pw>=length.min&
      is.na(width.cl)&
      is.na(height.tel)&
      is.na(radius)~"no",
    is.na(length.pw)&
      width.cl>=width.min&
      is.na(height.tel)&
      is.na(radius)~"no",
    is.na(length.pw)&
      is.na(width.cl)&
      height.tel>=height.min&
      is.na(radius)~"no",
    is.na(length.pw)&
      is.na(width.cl)&
      is.na(height.tel)&
      radius>=radius.min~"no",
    is.na(length.pw)&
      is.na(width.cl)&
      is.na(height.tel)&
      is.na(radius)~"no",
    TRUE~"yes"))

# look at how many we lose per plot/sampling/taxa
(size4<-size3%>%
    group_by(sampling,plotID,scientificName,too.small)%>%
    summarize(ns=n())%>%
    pivot_wider(names_from = too.small,values_from=ns,values_fill = 0))
size4<-size4%>%
    mutate(p.keep=no/(no+yes))%>%
  select(-no,-yes)

mpkeep<-size4%>%
  filter(!sampling %in% c("s1","s2"))%>%
  ungroup()%>%
  group_by(scientificName,plotID)%>%
  summarize(mp.keep=mean(p.keep))


# adjust bdiv data by %keep  
bdiv3<-left_join(bdiv2,size4)%>%
  filter(!is.na(UpdateTaxaID))%>%
  filter(UpdateTaxaID!="animal-0")%>%
  mutate(p.keep=ifelse(sampling %in% c("s1","s2"),1,p.keep))%>%
  left_join(mpkeep)%>%
  mutate(p.keep=ifelse(is.na(p.keep),mp.keep,p.keep),
         p.keep=ifelse(is.na(p.keep),1,p.keep),
         adj.n=total.n*p.keep)%>%
  select(-mp.keep,-p.keep)


ggplot(data=bdiv3%>%
         filter(total.n!=adj.n),aes(x=total.n,y=adj.n,color=scientificName))+
  geom_point()


# do a quick comparison of how it changes species richness answer
# get a dataset of all blockID/plotID and samplings that should be there
# so we can left_join to get the 0 spr

fulldat<-data.frame(plotID=unique(bdiv3$plotID),sampling=rep(c("s1","s2","s3","s4","s5"),48))

bdiv.adj<-bdiv3%>%
  ungroup()%>%
  select(-Surveyor,-total.n,-UpdateTaxaID)%>%
  group_by(sampling,blockID,plotID,scientificName)%>%
  summarize(adj.n=sum(adj.n))%>%
  pivot_wider(names_from = scientificName,values_from = adj.n,values_fill = 0)

bdiv.adj2<-bdiv.adj%>%
  select(sampling,blockID,plotID)

bdiv.adj2$adj.spr<-specnumber(bdiv.adj[,-1:-3])

bdiv.nadj<-bdiv3%>%
  ungroup()%>%
  select(-Surveyor,-UpdateTaxaID)%>%
  group_by(sampling,blockID,plotID,scientificName)%>%
  summarize(adj.n=sum(total.n))%>%
  pivot_wider(names_from = scientificName,values_from = adj.n,values_fill = 0)

bdiv.nadj2<-bdiv.adj%>%
  select(sampling,blockID,plotID)

bdiv.nadj2$orig.spr<-specnumber(bdiv.nadj[,-1:-3])

fulldat2<-left_join(fulldat,bdiv.nadj2)%>%
  left_join(bdiv.adj2)%>%
  mutate(adj.spr=ifelse(is.na(adj.spr),0,adj.spr),
         orig.spr=ifelse(is.na(orig.spr),0,orig.spr),
         delta.spr=adj.spr-orig.spr)%>%
  select(-blockID)%>%
  separate(plotID,into=c("blockID","ex"),sep=-2,remove=FALSE)

t.test(fulldat2$adj.spr,fulldat2$orig.spr)

ggplot(data=fulldat2,aes(color=sampling,y=delta.spr,x=ex))+
  geom_point(size=3,alpha=.2)+
  geom_hline(aes(yintercept=0))+
  facet_wrap(~blockID)

#output adjusted dataset

bdiv.out<-left_join(fulldat,bdiv3)%>%
  select(-blockID,-UpdateTaxaID,-Surveyor,-total.n)%>%
  separate(plotID,into=c("blockID","treat"),sep=-2,remove=FALSE)%>%
  mutate(time.var=case_when(
    sampling=="s1"~1,
    sampling=="s2"~2,
    sampling=="s3"~3,
    sampling=="s4"~4,
    sampling=="s5"~5,))%>%
  pivot_wider(names_from=scientificName,values_from=adj.n,values_fill = 0)
write.csv(bdiv.out,"wdata/wide_vissurvey_data.csv",row.names=F)
