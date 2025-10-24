# This script will  calculate dimensions of stability from Donohue et al. 2013 and begin analysis
# visual survey data

#Stephanie K. Archer 4/23/25

# load packages and download up to date data
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")
lp("lmerTest")
lp("EnvStats")
lp("vegan")
lp("GGally")
lp("factoextra")
library(codyn)



viswide$dummy<-1

vislong<-viswide%>%
  pivot_longer(-1:-6,names_to="taxa",values_to="abund")%>%
  mutate(pa=ifelse(abund==0,0,1))

plot.info<-vislong%>%  
  ungroup()%>%
  select(blockID,plotID)%>%
  distinct()

plot.info$Bay<-"SA"
plot.info$Bay[grep("SJ",plot.info$plotID)]<-"SJ"
plot.info$Scar<-"Scar"
plot.info$Scar[grep("UU",plot.info$plotID)]<-"No.scar"
plot.info$Scar[grep("UG",plot.info$plotID)]<-"No.scar"
plot.info$Graze<-"Graze"
plot.info$Graze[grep("SU",plot.info$plotID)]<-"No.graze"
plot.info$Graze[grep("UU",plot.info$plotID)]<-"No.graze"

# calculate univariate biodiversity metrics
vis.uni<-data.frame(viswide[,1:6],spr=specnumber(viswide[,c(-1:-6,-74)]),divers=diversity(viswide[,c(-1:-6,-74)]))
vis.uni$even=vis.uni$divers/log(vis.uni$spr)
vis.uni$even<-ifelse(is.na(vis.uni$even),0,vis.uni$even)

# temporal variability
# have to organize data a bit differently. Get total abundance for each plot/sampling period
stab<-vislong%>%
  filter(taxa!="dummy")%>%
  group_by(sampling,blockID,plotID)%>%
  summarize(total.abund=sum(abund))%>%
  ungroup()%>%
  left_join(vis.uni)

# visualize to see if there are temporal trends
ggplot(data=stab,aes(x=sampling))+
  geom_jitter(aes(y=even),width=.1)
            
stab.tcv<-stab%>%
  group_by(blockID,plotID)%>%
  summarize(temp.cva=cv(total.abund),
            temp.cvs=cv(spr),
            temp.cvd=cv(divers),
            temp.cve=cv(even))


# spatial variability

stab.scv<-vislong%>%
  filter(taxa!="dummy")%>%
  left_join(plot.info)%>%
  group_by(sampling,blockID,plotID,Scar,Graze)%>%
  summarize(total.abund=sum(abund))%>%
  ungroup()%>%
  left_join(vis.uni)%>%
  group_by(sampling,Scar,Graze)%>%
  summarize(space.cva=cv(total.abund),
            space.cvs=cv(spr),
            space.cvd=cv(divers),
            space.cve=cv(even))

# compositional turnover
viswide<-viswide[,colnames(viswide)!="NA"]

vis.pa<-bind_cols(viswide[,1:6],decostand(viswide[,-1:-6],method = "pa"))


vis.jac<-as.matrix(vegdist(vis.pa[vis.pa$sampling!="s1",-1:-6],method="jaccard"))
vis.pa.env<-vis.pa[,1:6]
vis.pa.env$nr<-rownames(vis.pa)

# write a function to extract dissimilarities
compturn<-function(pID,distmat,env){
  t1<-filter(env,plotID==pID)
  return(data.frame(plotID=pID,
                 s2.s3=distmat[t1$nr[2],t1$nr[1]],
                 s3.s4=distmat[t1$nr[3],t1$nr[2]],
                 s3.s4=distmat[t1$nr[4],t1$nr[3]]))
}

cturn<-data.frame(plotID=plot.info$plotID,s2.s3=NA,s3.s4=NA,s4.s5=NA)
for(i in 1:48)cturn[i,]<-compturn(pID=cturn$plotID[i],distmat = vis.jac,env=vis.pa.env)

cturn<-left_join(plot.info,cturn)

# number of extinctions and invasions

# write function to do this
ext.inv<-function(pID,pa.dat,n1,n2){
  t1<-filter(pa.dat,plotID==pID)%>%
    filter(sampling %in% c("s1","s5"))
  t2<-t1[2,n1:n2]-t1[1,n1:n2]
  return(data.frame(plotID=pID,
                    extinctions=rowSums(ifelse(t2<0,1,0)),
                    invasions=rowSums(ifelse(t2>0,1,0))))
}

cei<-data.frame(plotID=plot.info$plotID,extinctions=NA,invasions=NA)
for(i in 1:48)cei[i,]<-ext.inv(pID=cei$plotID[i],pa.dat=vis.pa,n1=-1,n2=-6)

cei<-left_join(plot.info,cei)

# calculate resistance

vis.bray<-vegdist(viswide[,-1:-6],method="bray")
vis.mds<-metaMDS(vis.bray)
plot(vis.mds)
vis.ris<-data.frame(viswide[,1:6]%>%
                      left_join(plot.info),
                    scores(vis.mds,choices=c(1,2),display="sites"))

vis.cent<-vis.ris%>%
  filter(sampling=="s1")%>%
  group_by(Scar,Graze,Bay)%>%
  summarize(cx=mean(NMDS1),
            cy=mean(NMDS2))


vis.ris<-vis.ris%>%
  filter(sampling!="s1")%>%
  left_join(vis.cent)%>%
  mutate(resistance=1/sqrt((NMDS1-cx)^2+(NMDS2-cy)^2))

# now combine into a single dataset
# also bring in pre and end shoot densities to use as covariates
sg<-sd<-read_xlsx(paste0("odata/downloaded_",Sys.Date(),"_EXPERIMENT - Seagrass and macroalgae Data.xlsx"),sheet=3)%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4]))%>%
  #filter(shoot.density!="NA")%>%
  mutate(shoot.density=as.numeric(shoot.density))%>%
  pivot_wider(names_from = taxa,values_from=shoot.density,values_fill=0)%>%
  pivot_longer('T':H,names_to="taxa",values_to = "shoot.density")%>%
  filter(taxa=="T")%>%
  group_by(blockID,plotID,sampling)%>%
  summarize(msd=mean(shoot.density,na.rm=T))%>%
  filter(sampling %in% c("s1","s4"))%>%
  mutate(nms=ifelse(sampling=="s1","start.sd","end.sd"))%>%
  ungroup()%>%
  select(-sampling)%>%
  pivot_wider(names_from = nms,values_from = msd,values_fill = 0)


stab.dat<-left_join(plot.info,stab.tcv)%>%
  left_join(cturn)%>%
  left_join(cei)%>%
  left_join(vis.ris)%>%
  left_join(stab.scv)%>%
  left_join(sg)%>%
  mutate(Turnover=(s2.s3+s3.s4+s4.s5)/3)%>%
  select(blockID,plotID,Bay,Scar,Graze,start.sd,end.sd,
         Temporal.a=temp.cva,Turnover,Extinctions=extinctions,
         Invasions=invasions,Resistance=resistance,Spatial.a=space.cva)

stab.dat$Scar<-factor(stab.dat$Scar,levels=c("No.scar","Scar"))
stab.dat$Graze<-factor(stab.dat$Graze,levels=c("No.graze","Graze"))
upper_panel_regression_line = function(x,y, ...){
  points(x,y,...)
  linear_regression = lm(y~x)
  linear_regression_line = abline(linear_regression)
}
pairs(stab.dat[,-1:-7],
      lower.panel = NULL,
      upper.panel = upper_panel_regression_line) # Add linear regression lines)

theme_set(theme_bw()+theme(panel.grid = element_blank()))
# overall correlations----
ggpairs(stab.dat[,-1:-7],
        #diag=list(continuous="bar"),
        lower=list(continuous="smooth"))

# control correlations----
ggpairs(stab.dat[grep("UU",stab.dat$plotID),-1:-7],
        #diag=list(continuous="bar"),
        lower=list(continuous="smooth"))

# scar correlations----
ggpairs(stab.dat[grep("SU",stab.dat$plotID),-1:-7],
        #diag=list(continuous="bar"),
        lower=list(continuous="smooth"))
#graze correlations ----
ggpairs(stab.dat[grep("UG",stab.dat$plotID),-1:-7],
        #diag=list(continuous="bar"),
        lower=list(continuous="smooth"))

# multiple correlations ----
ggpairs(stab.dat[grep("SG",stab.dat$plotID),-1:-7],
        #diag=list(continuous="bar"),
        lower=list(continuous="smooth"))
# stability pca

stab.stda<-decostand(stab.dat[,c(-1:-7,-9:-11,-16:-19)],method="standardize")
stab.stds<-decostand(stab.dat[,c(-1:-7,-8,-10:-11,-16:-19)],method="standardize")
stab.stdd<-decostand(stab.dat[,c(-1:-7,-8:-9,-11,-16:-19)],method="standardize")
stab.stde<-decostand(stab.dat[,c(-1:-7,-8:-10,-16:-19)],method="standardize")

stab.pcaa<-princomp(stab.stda)
stab.pcas<-princomp(stab.stds)
stab.pcad<-princomp(stab.stdd)
stab.pcae<-princomp(stab.stde)
plot(stab.pcaa)
plot(stab.pcas)
plot(stab.pcad)
plot(stab.pcae)

summary(stab.pcaa)

# use abundance for now
fviz_eig(stab.pcaa, addlabels = TRUE)
fviz_pca_var(stab.pcaa, col.var = "black")
# agree use first 3 axes
# see what is associated with each axis
stab.pcaa$loadings[, 1:3]
# axis 1 -
# positive correlation with temporal variation and invasion,
# negative correlation with extinctions
# axis 2 - 
# negative correlation with turnover and to a lesser extent extinctions
# axis 3 - 
# positively correlated with temporal variation and resistance

# this is what it looks like for all plots together - now lets compare
# treatments----
# control pca ----
stab.cont<-decostand(stab.dat[grep("UU",stab.dat$plotID),c(-1:-7,-9:-11,-16:-19)],method="standardize")
stab.cont.pca<-princomp(stab.cont)
plot(stab.cont.pca)
fviz_eig(stab.cont.pca, addlabels = TRUE)
fviz_pca_var(stab.cont.pca, col.var = "black")


# still 3 axes 
stab.cont.pca$loadings[, 1:3]
# axis 1 - 
# positively correlated with invasions
# negatively with resistance and extinctions
# axis 2 - 
# strongly correlated with temporal cv and then also turnover
# axis 3 - 
# negatively correlated with turnover
# positively correlated with temporal cv and invasions

# scarred pca ----
stab.scar<-decostand(stab.dat[grep("SU",stab.dat$plotID),c(-1:-7,-9:-11,-16:-19)],method="standardize")
stab.scar.pca<-princomp(stab.scar)
plot(stab.scar.pca)
fviz_eig(stab.scar.pca, addlabels = TRUE)
fviz_pca_var(stab.scar.pca, col.var = "black")


# only 2 axes
stab.scar.pca$loadings[, 1:2]
# axis 1 - 
# positively correlated temporal variation
# negatively with extinctions
# axis 2 - 
# correlated with turnover, invasions, and resistance

# grazing pca ----
stab.graze<-decostand(stab.dat[grep("UG",stab.dat$plotID),c(-1:-7,-9:-11,-16:-19)],method="standardize")
stab.graze.pca<-princomp(stab.graze)
plot(stab.graze.pca)
fviz_eig(stab.graze.pca, addlabels = TRUE)
fviz_pca_var(stab.graze.pca, col.var = "black")
# back to 3 axes
stab.graze.pca$loadings[, 1:3]
# axis 1 - 
# positive with temporal and resistance
# negative with extinctions
# axis 2 - 
# positive with invasions and turnover
# negative with resistance
# axis 3 - 
# negative with turnover (strong) and resistance

# multiple pca ----
stab.mult<-decostand(stab.dat[grep("SG",stab.dat$plotID),c(-1:-7,-9:-11,-16:-19)],method="standardize")
stab.mult.pca<-princomp(stab.mult)
plot(stab.mult.pca)
fviz_eig(stab.mult.pca, addlabels = TRUE)
fviz_pca_var(stab.mult.pca, col.var = "black")
# back to 3 axes
stab.mult.pca$loadings[, 1:3]


stab.dat$delta.sd<-stab.dat$end.sd-stab.dat$start.sd
# howstab.cont# how correlated are the treatments and delta.sd
ggpairs(stab.dat[,c(3:5,14)])

# do some tests
sd.aov<-lmer(delta.sd~Scar*Graze+(1|Bay)+(1|blockID),data=stab.dat)
plot(sd.aov)
summary(sd.aov)
anova(sd.aov)

plot(ggeffect(sd.aov,terms = "Graze"))
# grazed plots lost more shoot density than non-grazed plots

# long story short change in shoot density can stand in for grazing and bay 

# individual component analysis
par(mfrow=c(2,2))
temp.aov<-lmer(Temporal.a~Scar*Graze*Bay+(1|blockID),data = stab.dat)
plot(temp.aov)

temp.sd<-lmer(Temporal.a~Scar*delta.sd+(1|blockID),data=stab.dat)
plot(temp.sd)
lp("AICcmodavg")
AICc(temp.sd)
AICc(temp.aov)
# temp.sd is the better model
summary(temp.sd)
# the interaction between scar and delta.sd is significant
plot(ggeffect(temp.sd,terms = c("delta.sd","Scar")))
# when there is a scar - temporal variation increases as shoot density increases (due to recovery??)
# without a scar temporal variation is highest when seagrass density is lost

turn.aov<-lmer(Turnover~Scar*Graze*Bay+(1|blockID),data = stab.dat)
plot(turn.aov)
turn.sd<-lmer(Turnover~Scar*delta.sd+(1|blockID),data=stab.dat)
plot(turn.sd)
AICc(turn.aov)
AICc(turn.sd)
# turn sd is the better model
summary(turn.sd)
# but nothing is significant
plot(ggeffect(turn.sd,terms=c("delta.sd","Scar")))

#extinctions
ext.aov<-lmer(Extinctions~Scar*Graze*Bay+ (1|blockID),data = stab.dat)
plot(ext.aov)
ext.sd<-lmer(Extinctions~Scar*delta.sd+ (1|blockID),data = stab.dat)
plot(ext.sd)

AICc(ext.aov)
AICc(ext.sd)
# the treatment model is the best fit
summary(ext.aov)

#invasions
inv.aov<-lmer(Invasions~Scar*Graze*Bay+ (1|blockID),data = stab.dat)
plot(inv.aov)
inv.sd<-lmer(Invasions~Scar*delta.sd+ (1|blockID),data = stab.dat)
plot(inv.sd)
AICc(inv.aov)
AICc(inv.sd)

# the treatment model is the best fit
summary(inv.aov)
# no differences

#resistance
res.aov<-lmer(Resistance~Scar*Graze*Bay+ (1|blockID),data = stab.dat)
plot(res.aov)

res.sd<-lmer(Resistance~Scar*delta.sd+ (1|blockID),data = stab.dat)
plot(res.sd)


AICc(res.aov)
AICc(res.sd)
# sd model is the best fit
summary(res.sd)
# nothing is significant
plot(ggeffect(res.sd,terms=c("delta.sd","Scar")))

# spatial variation
sp.aov<-aov(space.cv~Scar*Graze,data=stab.scv)
plot(sp.aov)
anova(sp.aov)

# no difference
