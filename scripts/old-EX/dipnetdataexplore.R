# script to analyze dipnet data

# load scripts, packages, and bring in data

source("scripts/install_packages_function.R")
source("scripts/download_all_experiment_data-EX.R")
lp("tidyverse")
lp("readxl")
lp("vegan")
lp("lmerTest")

samplings<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)
dat1<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - LAB - Dipnet.xlsx"),sheet=2)
dat2<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - LAB - Dipnet.xlsx"),sheet=4)

# group things together then see if there are abundance to dry weight relationships for vouchered taxa

org.dat<-left_join(dat1,dat2[,1:2])%>%
  separate(voucher,into=c("ext","num"))%>%
  mutate(dw=as.numeric(dry.weight)-tin.weight,
         dw=ifelse(dw<0,0.001,dw),
         ww = wet.weight - tin.weight,
         ww=ifelse(ww<0,0.001,ww),
         abundance=as.numeric(abundance))

dw.na<-filter(org.dat,is.na(dw))
dw.ok<-filter(org.dat,!is.na(dw))

dw.na.tax<-unique(dw.na$UpdateTaxaID)

dw.rel<-data.frame(UpdateTaxaID=rep(NA,length(dw.na.tax)),a.Slope=NA,
                   w.Slope=NA)

for(i in 1:length(dw.na.tax)){
  t1<-filter(dw.ok,UpdateTaxaID==dw.na.tax[i])
  dw.rel$UpdateTaxaID[i]<-dw.na.tax[i]
  if(length(unique(t1$abundance))>1){
    t2<-summary(lm(I(dw-0)~0+abundance,data=t1))$coefficients
    dw.rel$a.Slope[i]<-t2[1,1]
  }
  if(length(unique(t1$ww))>1){
    t2<-summary(lm(I(dw-0)~0+ww,data=t1))$coefficients
    dw.rel$w.Slope[i]<-t2[1,1]
  }
}

dw.rel2<-filter(dw.rel,!is.na(w.Slope))

for(i in 1:nrow(dw.rel2)){
  t1<-filter(dw.na,UpdateTaxaID==dw.rel2$UpdateTaxaID[i])
  if(!is.na(dw.rel2$a.Slope[i]))dw<-dw.rel2$a.Slope[i]*t1$abundance
  if(is.na(dw.rel2$a.Slope[i]))dw<-dw.rel2$w.Slope[i]*t1$ww
  dw.na<-anti_join(dw.na,t1)
  t1$dw<-dw
  dw.na<-bind_rows(dw.na,t1)
}


org.dat2<-bind_rows(dw.na,dw.ok)%>%
  group_by(sample.date,blockID,plotID,UpdateTaxaID)%>%
  mutate(abund=sum(as.numeric(abundance),na.rm=T),
         ww = sum(ww,na.rm=T),
         dw=sum(dw,na.rm=T),
         dw=ifelse(dw==0,0.001,dw))%>%
  select(sample.date,lab.processor,blockID,plotID,UpdateTaxaID,abund,ww,dw)%>%
  distinct()%>%
  separate(plotID,into=c("bay","block","scar","graze"),sep=c(2,3,4),remove=F)%>%
  mutate(scar=ifelse(scar=="S","scarred","not-scarred"),
         graze=ifelse(graze=="G","grazed","not-grazed"))%>%
  mutate(sampling=case_when(
    sample.date >= samplings$Start.date[1] & sample.date <=samplings$End.date[1]~samplings$sampling[1],
    sample.date >= samplings$Start.date[2] & sample.date <=samplings$End.date[2]~samplings$sampling[2],
    sample.date >= samplings$Start.date[3] & sample.date <=samplings$End.date[3]~samplings$sampling[3]))

# now that data is organized - create data frames we can do community analysis on
dat.a<-org.dat2%>%
  filter(sampling=="s2")%>%
  select(-dw,-ww)%>%
  pivot_wider(names_from = UpdateTaxaID,values_from = abund,values_fill=0)

dat.env<-dat.a[,1:9]
dat.com.a<-dat.a[,-1:-9]
dat.com.a<-dat.com.a[,colnames(dat.com.a)!="NA"]
dat.env$taxa.rich<-specnumber(dat.com.a)
dat.env$total.abundance<-rowSums(dat.com.a)
dat.com.a$dummy<-1
dat.env<-dat.env%>%
  mutate(disturbed=case_when(
    scar=="scarred"&graze=="grazed"~"heavy",
    scar=="scarred"&graze!="grazed"~"moderate",
    scar!="scarred"&graze=="grazed"~"moderate",    
    scar!="scarred"&graze!="grazed"~"minimal"))
dat.env$disturbed<-factor(dat.env$disturbed,levels=c("minimal","moderate","heavy"))
dat.env$scar<-factor(dat.env$scar,levels=c("not-scarred","scarred"))
dat.env$graze<-factor(dat.env$graze,levels=c("not-grazed","grazed"))

#quick data vis
theme_set(theme_bw()+theme(panel.grid = element_blank()))
ggplot(data=dat.env)+
  geom_boxplot(aes(x=lab.processor,y=taxa.rich,color=lab.processor))

ggplot(data=dat.env)+
  geom_boxplot(aes(x=lab.processor,y=total.abundance,color=lab.processor))

ggplot(data=dat.env%>%
         filter(sampling=="s2"))+
  geom_boxplot(aes(y=taxa.rich,fill=graze,x=scar))+
  facet_wrap(~bay)

ggplot(data=dat.env%>%
         filter(sampling=="s2"))+
  geom_boxplot(aes(y=taxa.rich,fill=disturbed,x=bay))

ggplot(data=dat.env%>%
         filter(sampling=="s2"))+
  geom_boxplot(aes(y=total.abundance,fill=graze,x=scar))+
  facet_wrap(~bay)

ggplot(data=dat.env%>%
         filter(sampling=="s2"))+
  geom_boxplot(aes(y=total.abundance,fill=disturbed,x=bay))

# see if this is significant
s2.lmer<-lmer(taxa.rich~disturbed+bay+(1|blockID),
              data=dat.env)

plot(s2.lmer)

summary(s2.lmer)

lp("ggeffects")

s2.int<-ggpredict(s2.lmer,terms=c("disturbed"))
plot(s2.int)

# quick multivariate
dat.com.a.std<-decostand(dat.com.a,method="hellinger")
s2.mds<-metaMDS(dat.com.a,try=20,trymax = 1000)
plot(s2.mds)
s2.pca<-rda(dat.com.a.std)


dat.env<-bind_cols(dat.env,scores(s2.mds,display="sites"),scores(s2.pca,display="sites"))


ggplot(data=dat.env,aes(x=NMDS1,y=NMDS2,color=bay),)+
  geom_point(size=4,alpha=.4)+
  stat_ellipse()

ggplot(data=dat.env,aes(x=NMDS1,y=NMDS2,color=disturbed),)+
  geom_point(size=4,alpha=.4)+
  stat_ellipse()+
  facet_wrap(~bay)

ggplot(data=dat.env,aes(x=PC1,y=PC2,color=bay),)+
  geom_point(size=4,alpha=.4)+
  stat_ellipse()

ggplot(data=dat.env,aes(x=PC1,y=PC2,color=scar),)+
  geom_point(size=4,alpha=.4)+
  stat_ellipse()

