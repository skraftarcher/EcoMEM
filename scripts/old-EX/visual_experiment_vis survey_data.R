# Visualize visual survey data

# load packages and download up to date data
source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
2
lp("tidyverse")
lp("readxl")
lp("vegan")
lp("olsrr")

# bring in data
samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)

vis<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Visual Survey Data.xlsx"),sheet=2)

size<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Visual Survey Data.xlsx"),sheet=3)

tax<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - LAB - Dipnet.xlsx"),sheet=4)

# vislong----
vislong<-vis[-grep("egg",vis$taxa),]%>%
  filter(!is.na(taxa))%>%
  select(-scientific.name,-QAQC,-common.name,-Notes)%>%
  rename(taxaID=taxa)%>%
  left_join(tax)%>%
  group_by(date,blockID,plotID,UpdateTaxaID,Surveyor)%>%
  summarize(abund=sum(total.n))%>%
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
write.csv(vislong,"wdata/EcoMEM_visualBiodiversity.csv",row.names = FALSE)

#wide dataset----
viswide<-vislong%>%
  pivot_wider(names_from = UpdateTaxaID,values_from = abund,values_fill = 0)


# calculate univariate stats
vis.env<-viswide[,1:6]%>%
  separate(plotID,into = c("bay","block","scar","graze"),sep=c(2,3,4),remove = FALSE)%>%
  mutate(scar=ifelse(scar=="S","On scar","No scar"),
         graze=ifelse(graze=="G","Grazed","Not grazed"))
vis.com<-viswide[-1:-6]
vis.env$spr<-specnumber(vis.com[,-1])
vis.env$div<-diversity(vis.com[,-1])

theme_set(theme_bw()+theme(panel.grid=element_blank()))

ggplot(data=vis.env)+
  geom_boxplot(aes(x=scar,fill=graze,y=spr))+
  facet_grid(bay~sampling)

ggplot(data=vis.env)+
  geom_boxplot(aes(x=scar,fill=graze,y=div))+
  facet_grid(bay~sampling)

vis.com$dummy<-1
pc.hel<-decostand(vis.com[,-1],"hellinger")

vis.env2<-vis.env%>%
  mutate(scar2=ifelse(scar=="On scar",1,0),
         graze2=ifelse(graze=="Grazed",1,0),
         bay2=ifelse(bay=="SJ",1,0))%>%
  select(scar2,graze2,bay2,time.var,Surveyor)


pc.pca<-rda(pc.hel~graze2*time.var*scar2*bay2+Surveyor,data=vis.env2)
plot(pc.pca)
summary(pc.pca)
RsquareAdj(pc.pca)

# Global test of the RDA result
anova(pc.pca, permutations=how(nperm=999))

# Global adjusted R^2
(R2a.all <- RsquareAdj(pc.pca)$adj.r.squared)

mod0 <- rda(pc.hel ~ 1, data=vis.env2)
step.forward <- ordistep(mod0,scope=formula(pc.pca), 
                         direction="forward", permutations=how(nperm=199))

step.backward <- ordistep(pc.pca, 
                         direction="backward", permutations=how(nperm=199))



vis.env<-data.frame(vis.env,scores(pc.viz,display="sites",choices = c(1,2)))

ggplot(data = vis.env)+
  geom_point(aes(x=NMDS1,y=NMDS2,color=scar),size=3,alpha=.5)+
  facet_grid(~bay)
