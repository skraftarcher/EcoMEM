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
lighttemp<-read.csv("wdata/loggerdata summarized by plotID and pre or post grazing between may and june.csv")

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
  select(-d34S_sheaths,-PS_sheaths)

oms1<-filter(om,sampling=="s1")%>%
  select(plotID,s1OM=OMg.pergdw)

oms2<-filter(om,sampling=="s2")%>%
  select(plotID,s2OM=OMg.pergdw)

omd<-left_join(oms2,oms1)%>%
  mutate(pd.om=(s2OM-s1OM)/s1OM)

sg1<-filter(sg,sampling=="s1")%>%
  group_by(plotID)%>%
  summarize(s1.pcover=mean(pcover,na.rm = T),
            s1.sdens=mean(sdensity,na.rm=T),
            s1.can=mean(canopy,na.rm=T))

sg2<-filter(sg,sampling=="s2")%>%
  group_by(plotID)%>%
  summarize(s2.pcover=mean(pcover,na.rm = T),
            s2.sdens=mean(sdensity,na.rm=T),
            s2.can=mean(canopy,na.rm=T))

sgd<-left_join(sg1,sg2)%>%
  mutate(pd.pcover=(s2.pcover-s1.pcover)/s1.pcover,
         pd.sdens=(s2.sdens-s1.sdens)/s1.sdens,
         pd.can=(s2.can-s1.can)/s1.can)

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

grow1<-filter(sg.grow,sampling=="s1")%>%
  select(plotID,s1grow=areal.grow.per.day.per.shoot)

grow2<-filter(sg.grow,sampling=="s2")%>%
  select(plotID,s2grow=areal.grow.per.day.per.shoot)

growd<-left_join(grow1,grow2)%>%
  mutate(pd.grow=(s2grow-s1grow)/s1grow)

cnp1<-filter(cnp,sampling=="s1")%>%
  select(plotID,s1pn=PN,s1pc=PC,s1pp=PP)

cnp2<-filter(cnp,sampling=="s2")%>%
  select(plotID,s2pn=PN,s2pc=PC,s2pp=PP)

dcnp<-left_join(cnp1,cnp2)%>%
  mutate(pd.pn=(s2pn-s1pn)/s1pn,
         pd.pc=(s2pc-s1pc)/s1pc,
         pd.pp=(s2pp-s1pp)/s1pp,
         s1cn=s1pc/s1pn,
         s2cn=s2pc/s2pn,
         pd.cn=(s2cn-s1cn)/s1cn)

light<-lighttemp%>%
  select(plotID,prepost,m.light)%>%
  pivot_wider(names_from = prepost,values_from=m.light)

# build master dataset
init<-left_join(pinfo,core.int)%>%
  left_join(omd)%>%
  left_join(sgd)%>%
  left_join(bm1)%>%
  left_join(growd)%>%
  left_join(dcnp)%>%
  left_join(light)

write.csv(init,"wdata/change from s1 to s2.csv",row.names = F)
