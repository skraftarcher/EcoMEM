# organize all core data

# load packages and download up to date data----
source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2
lp("tidyverse")
lp("readxl")


# bring in data----
samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)
cor.bm<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass Core Data.xlsx"),sheet=4)
cor.mph<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass Core Data.xlsx"),sheet=3)

# organize data----
cor.bm2<-cor.bm%>%
  filter(sample.type!="animals")%>%
  mutate(sampling=case_when(
    sample.date >= samplings$Start.date[1] & sample.date <=samplings$End.date[1]~samplings$sampling[1],
    sample.date >= samplings$Start.date[2] & sample.date <=samplings$End.date[2]~samplings$sampling[2],
    sample.date >= samplings$Start.date[3] & sample.date <=samplings$End.date[3]~samplings$sampling[3],
    sample.date >= samplings$Start.date[4] & sample.date <=samplings$End.date[4]~samplings$sampling[4],
    sample.date >= samplings$Start.date[5] & sample.date <=samplings$End.date[5]~samplings$sampling[5]),
    time.var=case_when(
      sampling=="s1"~1,
      sampling=="s2"~2,
      sampling=="s3"~3,
      sampling=="s4"~4,
      sampling=="s5"~5,
    ),
    dry.g2=ifelse(is.na(dry.g.two),dry.g,dry.g.two),
    bm.g=dry.g2-foil.g,
    bm.g=ifelse(bm.g<=0,0.0001,bm.g))%>%
  group_by(sampling,time.var,plot,sample.type,sg.sp)%>%
  summarize(bm.g=sum(bm.g,na.rm = T),
            ns=n())

# things that have two foils should be QAQC'd again
write.csv(filter(cor.bm2,ns>1),"wdata/seagrasscore_toqaqc.csv",row.names = FALSE)

# now look for missing and/or incorrect plots
sg.plots<-cor.bm2%>%
  ungroup()%>%
  select(sampling,plot)%>%
  distinct()%>%
  mutate(link=1)%>%
  pivot_wider(names_from = sampling,values_from = link)

# there are several data issues that need to be cleaned up, re-run code when done

cor.bm3<-cor.bm2%>%
  select(-ns)%>%
  pivot_wider(names_from = sample.type,values_from=bm.g,values_fill = NA)

cor.bm3$above.ground<-rowSums(cor.bm3[,c(6,11,15)],na.rm = T)
cor.bm3$below.ground<-rowSums(cor.bm3[,c(9,10)],na.rm = T)

cor.bm3<-cor.bm3%>%
  pivot_longer(-1:-4, names_to = "sample.type",values_to = "bm.g")%>%
  filter(!is.na(bm.g))

cor.bm3$bay<-"St. Andrew"
cor.bm3$bay[grep(cor.bm3$plot,pattern="SJ")]<-"St. Joe"

cor.bm3$scar<-"Scar"
cor.bm3$scar[grep(cor.bm3$plot,pattern="UG")]<-"No scar"
cor.bm3$scar[grep(cor.bm3$plot,pattern="UU")]<-"No scar"

cor.bm3$graze<-"Graze"
cor.bm3$graze[grep(cor.bm3$plot,pattern="SU")]<-"No graze"
cor.bm3$graze[grep(cor.bm3$plot,pattern="UU")]<-"No graze"


cor.bm4<-cor.bm3%>%
  separate(plot,into = c("blockID","pt"),sep=3,remove=FALSE)%>%
  ungroup()%>%
  select(blockID,plotID=plot,bay,scar,graze,sampling,sg.sp,sample.type,bm.g)

# save dataset
write.csv(cor.bm4,"wdata/seagrass_core_biomass.csv",row.names = F)

# calculate change dataset
s1.bm<-filter(cor.bm4,sampling=="s1")%>%
  rename(pre.bm.g=bm.g)%>%
  select(-sampling)
s5.bm<-filter(cor.bm4,sampling!="s1")%>%
  rename(post.bm.g=bm.g)%>%
  select(-sampling)

bm<-full_join(s1.bm,s5.bm)%>%
  mutate(pre.bm.g=ifelse(is.na(pre.bm.g),0,pre.bm.g),
         post.bm.g=ifelse(is.na(post.bm.g),0,post.bm.g),
         delta.bm.g=post.bm.g-pre.bm.g)

write.csv(bm,"wdata/change_core_biomass.csv",row.names = F)

# now organize morphometrics

cor.mph2<-cor.mph%>%
  mutate(sampling=case_when(
    collect.date >= samplings$Start.date[1] & collect.date <=samplings$End.date[1]~samplings$sampling[1],
    collect.date >= samplings$Start.date[2] & collect.date <=samplings$End.date[2]~samplings$sampling[2],
    collect.date >= samplings$Start.date[3] & collect.date <=samplings$End.date[3]~samplings$sampling[3],
    collect.date >= samplings$Start.date[4] & collect.date <=samplings$End.date[4]~samplings$sampling[4],
    collect.date >= samplings$Start.date[5] & collect.date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  group_by(plot,sampling,species.id,ss.num)%>%
  mutate(n.leaves=max(leaf.num))%>%
  ungroup()%>%
  group_by(plot,sampling,species.id)%>%
  summarize(n.shoots=max(ss.num),
            m.leaves=mean(n.leaves),
            max.leaves=max(n.leaves),
            min.leaves=min(n.leaves),
            m.length=mean(leaf.length.cm,na.rm = T),
            min.length=min(leaf.length.cm,na.rm = T),
            max.length=max(leaf.length.cm,na.rm = T),
            m.width=mean(leaf.width.cm,na.rm = T),
            min.width=min(leaf.width.cm,na.rm = T),
            max.width=max(leaf.width.cm,na.rm = T))%>%
  mutate(across(n.shoots:max.width,~ifelse(is.na(.),0,.)),
         across(n.shoots:max.width,~ifelse(.==Inf|.==-Inf,0,.)))%>%
  separate(plot,into = c("blockID","pt"),sep=3,remove=F)

cor.mph2$bay<-"St. Andrew"
cor.mph2$bay[grep(cor.mph2$plot,pattern="SJ")]<-"St. Joe"

cor.mph2$scar<-"Scar"
cor.mph2$scar[grep(cor.mph2$plot,pattern="UG")]<-"No scar"
cor.mph2$scar[grep(cor.mph2$plot,pattern="UU")]<-"No scar"

cor.mph2$graze<-"Graze"
cor.mph2$graze[grep(cor.mph2$plot,pattern="SU")]<-"No graze"
cor.mph2$graze[grep(cor.mph2$plot,pattern="UU")]<-"No graze"

cor.mph3<-cor.mph2[,c(2,1,16:18,4:15)]%>%
  rename(plotID=plot,sg.sp=species.id)
write.csv(cor.mph3,"wdata/core morphometrics.csv",row.names = F)

# now calculate change dataset----

mph1<-filter(cor.mph3,sampling=="s1")%>%
  ungroup()%>%
  select(-sampling)
mph5<-filter(cor.mph3,sampling=="s5")%>%
  ungroup()%>%
  select(-sampling)

colnames(mph1)[-1:-6]<-paste0("pre_",colnames(mph1[,-1:-6]))
colnames(mph5)[-1:-6]<-paste0("post_",colnames(mph5[,-1:-6]))

delta.mph<-full_join(mph1,mph5)%>%
  mutate(across(pre_n.shoots:post_max.width,~ifelse(is.na(.),0,.)))%>%
  mutate(delta.n.shoots=post_n.shoots-pre_n.shoots,
         delta.n.leaves=post_m.leaves-pre_m.leaves,
         delta.max.length=post_max.length-pre_max.length,
         delta.max.width=post_max.width-pre_max.width,
         delta.m.length=post_m.length-pre_m.length,
         delta.m.width=post_m.width-pre_m.width,
         delta.min.width=post_min.width-pre_min.width)

write.csv(delta.mph,"wdata/change core morpho.csv",row.names = F)
