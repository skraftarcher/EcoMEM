# script to visualize seagrass data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")

pc<-read_xlsx("odata/downloaded_2024-05-14_EXPERIMENT - Seagrass and macroalgae Data.xlsx",sheet=2)
sd<-read_xlsx("odata/downloaded_2024-05-14_EXPERIMENT - Seagrass and macroalgae Data.xlsx",sheet=3)
can<-read_xlsx("odata/downloaded_2024-05-14_EXPERIMENT - Seagrass and macroalgae Data.xlsx",sheet=4)


# plot Thalassia percent cover

ggplot(pc%>%
         filter(taxa=="T")%>%
         mutate(pc=100*(as.numeric(percent.cover)/25))%>%
         separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
         mutate(scar=ifelse(scar=="U","Not on scar","On Scar")))+
  geom_violin(aes(x=pc,y=scar,fill=scar))+
  facet_wrap(~blockID)


# organize sd data
sd2<-sd%>%
  pivot_wider(names_from = taxa,values_from=shoot.density,values_fill=0)%>%
  select(-'NA')%>%
  pivot_longer('T':H,names_to="taxa",values_to = "shoot.density")%>%
  separate(plotID,into=c("b","scar","graze"),sep=c(3,4),remove=FALSE)%>%
  mutate(scar=ifelse(scar=="U","Not on scar","On Scar"))

ggplot(data=sd2%>%filter(taxa=="T"))+
  geom_violin(aes(y=scar,x=shoot.density,fill=scar))+
  facet_wrap(~blockID)+
  ggtitle("Thalassia")

ggplot(data=sd2%>%filter(taxa=="S"))+
  geom_violin(aes(y=scar,x=shoot.density,fill=scar))+
  facet_wrap(~blockID)+
  ggtitle("Syringodium")

ggplot(data=sd2%>%filter(taxa=="H"))+
  geom_violin(aes(y=scar,x=shoot.density,fill=scar))+
  facet_wrap(~blockID)+
  ggtitle("Halodule")

