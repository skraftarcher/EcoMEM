# analyze visual survey data----------------------------------------------------
# look at species richness, diversity, and evenness w the change in shoot density 
# interacting with scar and sampling (2 and 5)

# get community data
dat<-read.csv("wdata/wide_vissurvey_data.csv")
com<-dat[,-1:-5]

#look at top five most abundant taxa
abundance<-colSums(com, na.rm=T)
top_abund<-sort(abundance, decreasing = T)

print(head(top_abund, 5))

#look at top five most prevalent taxa
preval<-colSums(com > 0, na.rm=T)
top_preval<-sort(preval, decreasing = T)

print(head(top_preval, 5))

#see what taxa weren't present in s2 but are in s5
bounce_back_analysis <- dat%>%
  pivot_longer(cols=-colnames(dat[,1:5]), names_to = "taxa", values_to = "count")%>%
  group_by(sampling, taxa) %>%
  summarize(total_at_time = sum(count, na.rm = TRUE), .groups = "drop")%>%
  filter(sampling %in% c("s2", "s5"))%>%
  pivot_wider(names_from = sampling, values_from = total_at_time) %>%
  filter(s2 == 0, s5 > 0)

print("Taxa that were completely absent in S2 but returned in S5:")
print(bounce_back_analysis, n=22)

# get seagrass data
dat3<-read.csv("wdata/thalassia p cover shoot density and canopy by quadrat long.csv")%>%
  group_by(sampling,plotID)%>%
  summarize(mpcover=mean(pcover,na.rm=T),
            msdens=mean(sdensity,na.rm=T),
            mcan=mean(canopy,na.rm=T))

# treatments information
env<-dat[,1:5]%>%
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

# look at taxa responses spr and div_
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
spr<-lm(spr~scar+pchangesd+sampling,data=comd)

plot(spr)
summary(spr)
anova(spr)

# analyze diversity and change in shoot density----------
par(mfrow=c(2,2))
div<-lm(div~scar+pchangesd+sampling,data=comd)

plot(div)
summary(div)
anova(div)

# change in scallop abundance with change in shoot density----------
scallops<-lm(pchange~scar*pchangesd*sampling,data=comd)

par(mfrow=c(2,2))
plot(scallops)
summary(scallops)
anova(scallops)

lp("effectsize")
eta_squared(scallops, partial = TRUE)

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
glmm.resids<-function(deca2){
  t1 <- simulateResiduals(deca2)
  print(testDispersion(t1))
  print(testZeroInflation(t1))
  plot(t1)
}
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

# generating figures-------------------------------------------------------------

# species richness and diversity versus shoot density---------------
ggplot(data=comd,aes(x=pchangesd,y=spr))+
  geom_point()+
  geom_smooth(aes(group = 1), method = "lm", color = "black", linetype = "dashed")+
  labs(
    title = "Species Richness by Change in Shoot Density",
    subtitle = "Significant positive relationship between density and richness (t=2.58,p=0.012)",
    x = "Percent Change in Shoot Density",
    y = "Species Richness"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# diversity versus sampling
ggplot(data=comd,aes(x=pchangesd,y=div))+
  geom_point()+
  geom_smooth(aes(group = 1), method = "lm", color = "black", linetype = "dashed")+
  labs(
    title = "Diversity by Change in Shoot Density",
    subtitle = "Significant positive relationship between density and diversity (t=2.03, p=0.046)",
    x = "Percent Change in Shoot Density",
    y = "Diversity"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# species richness and diversity versus sampling------------------------
lp("ggpubr")

# boxplots
# species richness
ggplot(data=comd,aes(x=sampling,y=spr, fill=sampling))+
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(color = "black", width = 0.1, alpha = 0.3)+
  stat_compare_means(
    comparisons = list(c("s2", "s5")), 
    method = "t.test", 
    label = "p.signif"
  )+
  scale_x_discrete(labels = c("s2" = "One month", 
                              "s5" = "13 months"))+
  labs(
    title = "Species Richness Across Sampling Periods",
    subtitle = "Significant increase observed from Sampling 2 to Sampling 5 (p < 0.001)",
    x = "Time Post-Disturbance",
    y = "Species Richness"
  ) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# diversity
ggplot(data=comd,aes(x=sampling,y=div, fill=sampling))+
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(color = "black", width = 0.1, alpha = 0.3)+
  stat_compare_means(
    comparisons = list(c("s2", "s5")), 
    method = "t.test", 
    label = "p.signif"
  )+
  scale_x_discrete(labels = c("s2" = "One month", 
                              "s5" = "13 months"))+
  labs(
    title = "Diversity Across Sampling Periods",
    subtitle = "Significant increase observed from Sampling 2 to Sampling 5 (p < 0.001)",
    x = "Time Post-Disturbance",
    y = "Diversity"
  ) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# three way interaction faceted scatter plot
sample_names <- c(
  "s2" = "One month post-disturbance",
  "s5" = "13 months post-disturbance"
)
ggplot(data=comd,aes(x=pchangesd, y=pchange, color=scar, fill=scar))+
  geom_jitter(alpha = 0.5, width = 0.2)+
  geom_point()+
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE)+
  facet_wrap(~ sampling, scales = "fixed", labeller = as_labeller(sample_names))+
  scale_color_manual(
    values = c("no scar" = "#785EF0", "scar" = "#FFB000"),
    labels = c("No Scar", "Scarred")
  ) +
  scale_fill_manual(
    values = c("no scar" = "#785EF0", "scar" = "#FFB000"),
    labels = c("No Scar", "Scarred")
  ) +
  labs(
    title = "Interactive Effects of Seagrass Density and Scarring over Time on Scallop Abundance Change",
    subtitle = "Significant 3-way Interaction: F = 4.381, p < 0.05",
    #x = "Percent Change in Shoot Density",
    x = expression(Percent ~ Change ~ "in" ~ Shoot ~ Density: eta[p]^2 == 0.16),
    y = "Percent Change in Scallop Abundance",
    #color = "Seagrass Treatment",
    #fill = "Seagrass Treatment",
    color = expression(Seagrass ~ Treatment: eta[p]^2 < 0.005),
    fill = expression(Seagrass ~ Treatment: eta[p]^2 < 0.005)
  ) +
  #scale_x_continuous(sec.axis = sec_axis(~ . , name = "Recovery Time", breaks = NULL, labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = expression(Recovery ~ Time: eta[p]^2 == 0.03), breaks = NULL, labels = NULL))+
  theme_minimal()+
  theme(legend.position = "bottom",
        strip.text = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
