# analyze visual survey data----------------------------------------------------
# look at species richness, diversity, and evenness w the change in shoot density 
# interacting with scar and sampling (2 and 5)

# get community data
dat<-read.csv("wdata/wide_vissurvey_data.csv")
com<-dat[,-1:-4]

# get seagrass data
dat3<-read.csv("wdata/thalassia p cover shoot density and canopy by quadrat long.csv")%>%
  group_by(sampling,plotID)%>%
  summarize(mpcover=mean(pcover,na.rm=T),
            msdens=mean(sdensity,na.rm=T),
            mcan=mean(canopy,na.rm=T))

# treatments information
env<-dat[,1:4]%>%
  mutate(graze=case_when(
    treat=="UU"~"no graze",
    treat=="UG"~"graze",
    treat=="SU"~"no graze",
    treat=="SG"~"graze"))%>%
  mutate(scar=case_when(
    treat=="UU"~"no scar",
    treat=="UG"~"no scar",
    treat=="SU"~"scar",
    treat=="SG"~"scar"))%>%
  select(-treat)%>%
  left_join(dat3)

# calculate species richness, diversity, and evenness
env$spr<-specnumber(com)
env$div<-diversity(com)
env$even<-diversity(com)/log(specnumber(com))

# look at taxa responses
com2<-dat%>%
  pivot_longer(-1:-5,names_to = "taxa",values_to="abund")%>%
  left_join(env)

# get sampling 1 out and change abundances to "preabund"
s1com<-com2%>%
  filter(sampling=="s1")%>%
  dplyr::select(plotID,taxa,preabund=abund,
                premsdens=msdens,
                prespr=spr,
                prediv=div,
                preeven=even)

comd<-com2%>%
  filter(sampling!="s1")%>%
  left_join(s1com)%>%
  mutate(pchange=100*((abund-preabund)/(preabund)),
         pchange=ifelse(preabund==0,abund*100,pchange),
         pchangesd=100*((msdens-premsdens)/(premsdens)),
         pchangespr=100*((spr-prespr)/(prespr)),
         pchangespr=ifelse(prespr==0,spr*100,pchangespr),
         pchangediv=100*((div-prediv)/(prediv)),
         pchangediv=ifelse(prediv==0,div*100,pchangediv),
         pchangeeven=100*((even-preeven)/(preeven)),
         pchangeeven=ifelse(preeven==0,even*100,pchangeeven))%>%
  filter(taxa=="Argopecten.irradians")%>%
  filter(sampling %in% c("s2","s5"))

# analyze species richness and change in shoot density----------
par(mfrow=c(2,2))
spr<-lm(spr~scar*pchangesd*sampling,data=comd)

plot(spr)
summary(spr)
anova(spr)

# analyze diversity and change in shoot density----------
par(mfrow=c(2,2))
div<-lm(div~scar*pchangesd*sampling,data=comd)

plot(div)
summary(div)
anova(div)

# change in scallop abundance with change in shoot density----------
scallops<-aov(pchange~scar*pchangesd*sampling,data=comd)

par(mfrow=c(2,2))
plot(scallops)
summary(scallops)

# analyze species evenness and change in shoot density----------

# analyze dipnet data-----------------------------------------------------------
# load data
dip<-read.csv("wdata/feb32026_dipnet.csv")

dip2<-dip%>%
  group_by(plotID,sampling,time.var,scientificName)%>%
  summarize(abundance=sum(abundance,na.rm=T))

dip2.wide<-dip2%>%
  group_by(plotID,sampling,time.var,scientificName)%>%
  pivot_wider(names_from=scientificName,values_from=abundance,values_fill = 0)

dip.com<-dip2.wide[,-1:-2]

# treatments information
dip.env<-dip2.wide[,1:2]%>%
  mutate(graze=case_when(
    endsWith(plotID, "UU")~"no graze",
    endsWith(plotID,"UG")~"graze",
    endsWith(plotID,"SU")~"no graze",
    endsWith(plotID,"SG")~"graze"))%>%
  mutate(scar=case_when(
    endsWith(plotID,"UU")~"no scar",
    endsWith(plotID,"UG")~"no scar",
    endsWith(plotID,"SU")~"scar",
    endsWith(plotID,"SG")~"scar"))%>%
  left_join(dat3)

# calculate species richness, diversity, and evenness
dip.env$spr<-specnumber(dip.com)
dip.env$div<-diversity(dip.com)
dip.env$even<-diversity(dip.com)/log(specnumber(dip.com))

# look at taxa responses
dip.com2<-dip2.wide%>%
  pivot_longer(-1:-3,names_to = "taxa",values_to="abund")%>%
  left_join(dip.env)

# get dataset for species richness and diversity analysis of sampling 2 only
# and get pchangesd data from visual survey analysis
dip.comd <- dip.com2%>%
  ungroup()%>%
  left_join(comd[, c("plotID", "sampling", "time.var", "pchangesd")],
        by = c("plotID", "sampling", "time.var"))

# analyze species richness and change in shoot density----------

par(mfrow=c(2,2))
spr<-lm(spr~scar*pchangesd,data=dip.comd%>%
          filter(taxa == "Acteocina canaliculata", sampling == "s2"))

plot(spr)
summary(spr)
anova(spr)

# analyze diversity and change in shoot density----------
par(mfrow=c(2,2))
div<-lm(div~scar*pchangesd,data=dip.comd%>%
          filter(taxa == "Acteocina canaliculata", sampling == "s2"))

plot(div)
summary(div)
anova(div)

lp("DHARMa")
lp("glmmTMB")
# change in mud crab abundance with change in shoot density----------
deca2<-glmmTMB(abund~scar*pchangesd,data=dip.comd%>%
                filter(taxa == "Rhithropanopeus cf harrisii", sampling == "s2"),
               family = "poisson")

glmm.resids(deca2)
summary(deca2)

# change in lunar dove snail abundance with change in shoot density----------
gast20<-glmmTMB(abund~scar*pchangesd,data=dip.comd%>%
             filter(taxa == "Astyris lunata", sampling == "s2"),
             family = "poisson")

glmm.resids(gast20)
summary(gast20)

# change in gast 4 abundance with change in shoot density----------
gast4<-glmmTMB(abund~scar*pchangesd,data=dip.comd%>%
             filter(taxa == "Caecum cf pulchellum", sampling == "s2"),
             family = "nbinom1")

glmm.resids(gast4)
summary(gast4)

# change in common eastern nassau abundance with change in shoot density----------
gast15<-glmmTMB(abund~scar*pchangesd,data=dip.comd%>%
             filter(taxa == "Phrontis vibex", sampling == "s2"),
             family = "nbinom1")

glmm.resids(gast15)
summary(gast15)
