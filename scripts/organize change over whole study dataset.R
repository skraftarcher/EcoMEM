# organize dataset to look at things in pre-sampling, and at second sampling

source("scripts/install_packages_function.R")

lp("tidyverse")

core.iso<-read.csv("wdata/core_isotopes.csv")
om<-read.csv("wdata/sediment OM data.csv")
sg<-read.csv("wdata/thalassia p cover shoot density and canopy by quadrat long.csv")
bm<-read.csv("wdata/seagrass_core_biomass.csv")
sg.grow<-read.csv("wdata/seagrass growth per day.csv")
cnp<-read.csv("wdata/cnp.csv")%>%
  select(-date)
pinfo<-read.csv("odata/allplotinfo.csv")
lighttemp<-read.csv("wdata/loggerdata summarized by block and sampling.csv")
wq<-read.csv("wdata/water quality combined.csv")

# organize each data set to include the first and second sampling/changes between the first and second sampling

core.int<-core.iso%>%
  filter(sampling!="final")%>%
  select(-sID,-date,-sampling,-scar,-graze,-bay)%>%
  distinct()%>%
  pivot_wider(names_from=component,values_from=c("d15N",
                                                 "PN",
                                                 "d13C",
                                                 "PC",
                                                 "CN",
                                                 "d34S",
                                                 "PS"))%>%
  select(plotID,
         pre_d15N_leaves=d15N_leaves,
         pre_d15N_rhizomes =d15N_rhizomes,
         pre_d34S_leaves=d34S_leaves,
         pre_d34S_roots=d34S_roots,
         pre_d34S_rhizomes=d34S_rhizomes,
         pre_PS_leaves=PS_leaves,
         pre_PS_roots=PS_roots,
         pre_PS_rhizomes=PS_rhizomes)

core.fin<-core.iso%>%
  filter(sampling=="final")%>%
  select(-sID,-date,-sampling,-scar,-graze,-bay)%>%
  distinct()%>%
  pivot_wider(names_from=component,values_from=c("d15N",
                                                 "PN",
                                                 "d13C",
                                                 "PC",
                                                 "CN",
                                                 "d34S",
                                                 "PS"))%>%
  select(plotID,
         final_d15N_leaves=d15N_leaves,
         final_d15N_rhizomes = d15N_rhizomes,
         final_d34S_leaves=d34S_leaves,
         final_d34S_roots=d34S_roots,
         final_d34S_rhizomes=d34S_rhizomes,
         final_PS_leaves=PS_leaves,
         final_PS_roots=PS_roots,
         final_PS_rhizomes=PS_rhizomes)


oms1<-filter(om,sampling=="s1")%>%
  select(plotID,s1OM=OMg.pergdw)

oms2<-filter(om,sampling!="s1")%>%
  select(plotID,sampling,OM=OMg.pergdw)

omd<-left_join(oms2,oms1)%>%
  mutate(pd.om=(OM-s1OM)/s1OM)

sg1<-filter(sg,sampling=="s1")%>%
  group_by(plotID)%>%
  summarize(s1.pcover=mean(pcover,na.rm = T),
            s1.sdens=mean(sdensity,na.rm=T),
            s1.can=mean(canopy,na.rm=T))

sg2<-filter(sg,sampling!="s1")%>%
  group_by(plotID,sampling)%>%
  summarize(pcover=mean(pcover,na.rm = T),
            sdens=mean(sdensity,na.rm=T),
            can=mean(canopy,na.rm=T))

sgd<-left_join(sg1,sg2)%>%
  mutate(pd.pcover=(pcover-s1.pcover)/s1.pcover,
         pd.sdens=(sdens-s1.sdens)/s1.sdens,
         pd.can=(can-s1.can)/s1.can)

bm1<-filter(bm,sampling=="s1")%>%
  mutate(ab=case_when(
    sample.type %in% c("leaf.alive","sheath.alive","buds.alive")~"above",
    sample.type %in% c("thizome.alive","roots.alive")~"below",
    sample.type == "macroalgae"~"macroalgae"))%>%
  filter(!is.na(ab))%>%
  group_by(plotID,ab)%>%
  summarize(bm.g=sum(bm.g,na.rm=T))%>%
  pivot_wider(names_from=ab,values_from = bm.g,values_fill=0)%>%
  mutate(a2b=above/below)

bm2<-filter(bm,sampling!="s1")%>%
  mutate(ab=case_when(
    sample.type %in% c("leaf.alive","sheath.alive","buds.alive")~"above",
    sample.type %in% c("thizome.alive","roots.alive")~"below",
    sample.type == "macroalgae"~"macroalgae"))%>%
  filter(!is.na(ab))%>%
  group_by(plotID,ab)%>%
  summarize(bm.g=sum(bm.g,na.rm=T))%>%
  pivot_wider(names_from=ab,values_from = bm.g,values_fill=0)%>%
  mutate(finala2b=above/below)%>%
  rename(finalabove=above,finalbelow=below,finalmacro=macroalgae)
  

grow1<-filter(sg.grow,sampling=="s1")%>%
  select(plotID,s1grow=areal.grow.per.day.per.shoot)

grow2<-filter(sg.grow,sampling!="s1")%>%
  select(plotID,sampling,grow=areal.grow.per.day.per.shoot)

growd<-left_join(grow1,grow2)%>%
  mutate(pd.grow=(grow-s1grow)/s1grow)

cnp1<-filter(cnp,sampling=="s1")%>%
  select(plotID,s1pn=PN,s1pc=PC,s1pp=PP)

cnp2<-filter(cnp,sampling!="s1")%>%
  select(plotID,sampling,pn=PN,pc=PC,pp=PP)

dcnp<-left_join(cnp1,cnp2)%>%
  mutate(pd.pn=(pn-s1pn)/s1pn,
         pd.pc=(pc-s1pc)/s1pc,
         pd.pp=(pp-s1pp)/s1pp,
         s1cn=s1pc/s1pn,
         cn=pc/pn,
         pd.cn=(cn-s1cn)/s1cn)

light1<-filter(lighttemp,sampling=="s1")%>%
  select(blockID,prepost,s1.light=m.light,s1.temp=m.temp)%>%
  pivot_wider(names_from=prepost,values_from = c(s1.light,s1.temp))

light2<-filter(lighttemp,sampling!="s1")%>%
  select(blockID,sampling,m.light,m.temp)%>%
  distinct()%>%
  left_join(light1)





wq1<-filter(wq,sampling=="s1")%>%
  select(blockID,ph1=ph,orp1=orp,do1=do,nh3.mgl1=nh3.mgl,nox.ummol1=nox.ummol,po4p.mgl1=po4p.mgl)

wq2<-filter(wq,sampling!="s1")%>%
  select(blockID,sampling,ph,orp,do,nh3.mgl,nox.ummol,po4p.mgl)%>%
  left_join(wq1)


# build master dataset
init<-right_join(sgd,pinfo)%>%
  left_join(core.int)%>%
  left_join(core.fin)%>%
  left_join(omd)%>%
  left_join(bm1)%>%
  left_join(bm2)%>%
  left_join(growd)%>%
  left_join(dcnp)%>%
  left_join(light2)%>%
  left_join(wq2)

write.csv(init,"wdata/change over course of study.csv",row.names = F)
