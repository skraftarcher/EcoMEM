# create one big data set for looking at mechanism/correlation of differential resilience to loss of above-ground biomass

# load datasets and packages
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("vegan")

core.iso<-read.csv("wdata/core_isotopes.csv")
om<-read.csv("wdata/sediment OM data.csv")
sg<-read.csv("wdata/thalassia p cover shoot density and canopy by quadrat long.csv")
bm<-read.csv("wdata/change_core_biomass.csv")
sg.grow<-read.csv("wdata/seagrass growth per day.csv")
pore<-read.csv("wdata/porewater_sulfides_plots.csv")
cnp<-read.csv("wdata/cnp.csv")%>%
  select(-date)
pinfo<-read.csv("odata/allplotinfo.csv")


# organize individual datasets so they are ready to combine into 1
# can get rid of everything but plotID, sampling, and the data so that the pinfo can be used to standardize treatment, etc

colnames(core.iso)
core.iso<-core.iso%>%
  select(plotID,sampling,component,PC,PN,PS,CN,d13C,d15N,d34S)%>%
  mutate(sampling=ifelse(sampling=="final","s5","s1"))

colnames(om)
om<-om%>%
  select(plotID,sampling,OMg.pergdw,dw.wt)

colnames(sg)
sg<-sg%>%
  group_by(plotID,sampling)%>%
  summarize(pcover.sd=sd(pcover),
            pcover=mean(pcover,na.rm=T),
            sdensity.sd=sd(sdensity),
            sdensity=mean(sdensity,na.rm = T),
            canopy.sd=sd(canopy),
            canopy=mean(canopy,na.rm=T))

colnames(bm)
bm2<-bm%>%
  filter(sample.type %in% c("above.ground","below.ground"))%>%
  group_by(plotID,sample.type)%>%
  summarize(pre.bm.g=sum(pre.bm.g),
            post.bm.g=sum(post.bm.g),
            pchange.bm=ifelse(pre.bm.g==0,NA,(post.bm.g-pre.bm.g)/pre.bm.g))%>%
  mutate(sampling="s5")%>%
  rename(var=sample.type,s1_meas=pre.bm.g,meas=post.bm.g,pchange=pchange.bm)

bm.rat<-bm%>%
  filter(sample.type %in% c("above.ground","below.ground"))%>%
  group_by(blockID,plotID,sample.type)%>%
  summarize(pre.bm.g=sum(pre.bm.g,na.rm = T),
            post.bm.g=sum(post.bm.g,na.rm = T))%>%
  pivot_wider(names_from=sample.type,values_from=c(pre.bm.g,post.bm.g))%>%
  mutate(ab.rat.pre=pre.bm.g_above.ground/pre.bm.g_below.ground,
         ab.rat.post=post.bm.g_above.ground/post.bm.g_below.ground,
         pchange.ab.rat=ifelse(ab.rat.pre==0,NA,(ab.rat.post-ab.rat.pre)/ab.rat.pre))


colnames(sg.grow)
sg.grow<-sg.grow%>%
  select(plotID,sampling,n.shoots,areal.grow.per.day.per.shoot,g.sg.pershoot.perday)

colnames(pore)
pore<-pore%>%
  mutate(graze=ifelse(graze=="No.graze","U","G"),
         plotID=paste0(blockID,"U",graze))%>%
  group_by(plotID,sampling)%>%
  summarize(conc.sulf.um=mean(Concentration.um,na.rm=T),
            conc.sulf.sd=sd(Concentration.um,na.rm=T))

# organize a data set to calculate at resistance and resilience of sg variables
# look at things real quick to determine how to calculate things
theme_set(theme_bw()+theme(panel.grid = element_blank()))
ggplot(data = left_join(pinfo,sg),aes(x=sampling,group=plotID,color=scar))+
  geom_line(aes(y=canopy))+
  facet_grid(bay~graze)

ggplot(data = left_join(pinfo,sg.grow),aes(x=sampling,group=plotID,color=scar))+
  geom_line(aes(y=areal.grow.per.day.per.shoot))+
  facet_grid(bay~graze,scales="free")

# resistance is going to be calculated as the % change over time
# calculate resistance for variables
sg2<-full_join(pinfo,sg)%>%
  left_join(sg.grow)%>%
  left_join(cnp)%>%
  mutate(prod=((sdensity/(.1*.1))*(2*2)*areal.grow.per.day.per.shoot))%>%# units of prod are cm3 sg growth per plot per day
  select(-g.sg.pershoot.perday)%>%
  pivot_longer(-plotID:-sampling,names_to="var",values_to = "meas")

sg1<-filter(sg2,sampling=="s1")%>%
  rename(s1_meas=meas)%>%
  select(-sampling)

sg.o<-filter(sg2,sampling!="s1")%>%
  left_join(sg1)%>%
  mutate(pchange=(meas-s1_meas)/s1_meas)%>%
  bind_rows(full_join(pinfo,bm2))

# look at the % change in variables over time
ggplot(aes(x=sampling,y=pchange,group=plotID,color=graze),data=sg.o%>%filter(scar=="No.scar")%>%
         filter(!var %in% c("pcover.sd","sdensity.sd","canopy.sd",
                            "n.shoots","PN","PP","PC","areal.grow.per.day.per.shoot",
                            "prod","above.ground","below.ground")))+
  geom_hline(aes(yintercept = 0))+
  geom_line()+
  facet_grid(var~bay,scales="free")


# use % change in shoot density and % change in canopy height at sampling 5
# use pre-sampling above:below ground ratio, pre-epiphyte load, sediment organic matter, porewater sulfides, sulfide isotopes from pre-sampling in rhizomes
# and wet to dry weight ratios for sediment as explanatory variables

bm.e<-bm%>%
  filter(sample.type=="epiphytes")%>%
  select(-post.bm.g,-delta.bm.g,-bay,-scar,-graze,-sg.sp)%>%
  select(-sample.type)%>%
  rename(epi.bm=pre.bm.g)

bm.ab<-bm%>%
  filter(sample.type %in% c("above.ground","below.ground"))%>%
  select(-post.bm.g,-delta.bm.g,-bay,-scar,-graze,-sg.sp)%>%
  group_by(plotID,sample.type)%>%
  summarize(pre.bm.g=sum(pre.bm.g))%>%
  pivot_wider(names_from = sample.type,values_from=pre.bm.g)%>%
  mutate(above.below=above.ground/below.ground)

om2<-om%>%
  filter(sampling=="s1")%>%
  select(-sampling)

core<-core.iso%>%
  filter(sampling=="s1")%>%
  filter(component=="rhizomes")%>%
  select(-sampling,-component)

core2<-core.iso%>%
  filter(sampling=="s1")%>%
  filter(component=="leaves")%>%
  select(plotID,leaf.pc=PC,leaf.pn=PN)%>%
  distinct()
  
core3<-core.iso%>%
  filter(sampling=="s1")%>%
  filter(component=="roots")%>%
  select(plotID,root.d34S=d34S)%>%
  distinct()


resp<-sg.o%>%
  filter(sampling=="s5")%>%
  select(-s1_meas,-meas)%>%
  filter(var %in% c("sdensity","canopy","pcover"))%>%
  pivot_wider(names_from = var,values_from = pchange)%>%
  select(-sampling)

pore2<-pore[,-4]%>%
  pivot_wider(names_from=sampling,values_from=conc.sulf.um)%>%
  rename(sulf.s4=s4,sulf.s5=s5)
  

res.dat<-left_join(pinfo,resp)%>%
  left_join(core)%>%
  left_join(core2)%>%
  left_join(core3)%>%
  left_join(pore2)%>%
  left_join(om2)%>%
  left_join(bm.e)%>%
  left_join(bm.ab)%>%
  left_join(bm.rat[,c(1,2,9)])%>%
  filter(scar=="No.scar")

write.csv(res.dat,"wdata/resilience and sulfide.csv",row.names = F)
