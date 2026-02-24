# analyze visual survey data----------------------------------------------------
# evenness and percent change in shoot density
# using visual survey data organized from
# Stephanie Archer's organize_visual_survey_data.R
# and pc sd can data organized from
# Stephanie Arhcer's organize_pc_sd_can_data.R (w/o plotinfo)


#get community data
dat<-read.csv("wdata/wide_vissurvey_data.csv")
com<-dat[,-1:-4]

#get seagrass data
dat3<-read.csv("wdata/thalassia p cover shoot density and canopy by quadrat long.csv")%>%
  group_by(sampling,plotID)%>%
  summarize(mpcover=mean(pcover,na.rm=T),
            msdens=mean(sdensity,na.rm=T),
            mcan=mean(canopy,na.rm=T))

#treatments information
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

#calculating species number, diversity, and evenness
env$spr<-specnumber(com)-1
env$div<-diversity(com[,colnames(com)!="dummy"])
env$even<-diversity(com)/log(specnumber(com))

# look at taxa responses
com2<-dat%>%
  pivot_longer(-1:-4,names_to = "taxa",values_to="abund")%>%
  left_join(env)%>%
  mutate(graze=case_when(
    treat=="UU"~"no graze",
    treat=="UG"~"graze",
    treat=="SU"~"no graze",
    treat=="SG"~"graze"))%>%
  mutate(scar=case_when(
    treat=="UU"~"no scar",
    treat=="UG"~"no scar",
    treat=="SU"~"scar",
    treat=="SG"~"scar"))

# get sampling 1 out and change abundances to "preabund"

s1com<-com2%>%
  filter(sampling=="s1")%>%
  dplyr::select(plotID,taxa,preabund=abund,
                premsdens=msdens,
                preeven=even)

comd<-com2%>%
  filter(sampling!="s1")%>%
  left_join(s1com)%>%
  mutate(pchange=100*((abund-preabund)/(preabund)),
         pchange=ifelse(preabund==0,abund*100,pchange),
         pchangesd=100*((msdens-premsdens)/(premsdens)),
         pchangeeven=100*((even-preeven)/(preeven)),
         pchangeeven=ifelse(preeven==0,even*100,pchangeeven))

#look at abundance and seagrass shoots density----------------------------------

scallop_data <- comd %>%
  filter(taxa == "Argopecten.irradians", 
        sampling %in% c("s2", "s5"))

ggplot(scallop_data, aes(x = pchangesd, y = pchange)) +
  geom_point(aes(color = graze), alpha = 0.6) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~sampling) +
  labs(title = "Scallop Response to Percent Change in Seagrass Density",
       subtitle = "Comparing Early (s2) vs. Late (s5) Sampling Periods",
       x = "Percent Change in Seagrass Density (shoots/m2)",
       y = "Scallop Abundance Change") +
  theme_minimal()

#look at sampling effect--------------------------------------------------------
plot_data <- comd %>%
  filter(taxa == "Argopecten.irradians", sampling %in% c("s2", "s5")) %>%
  group_by(sampling, graze) %>%
  summarize(mean_p = mean(pchange, na.rm = TRUE),
            se = sd(pchange, na.rm = TRUE) / sqrt(n()))

ggplot(plot_data, aes(x = sampling, y = mean_p, group = graze, color = graze)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_p - se, ymax = mean_p + se), width = 0.1) +
  labs(title = "Consistent Grazing Effect Over Time",
       subtitle = "Significant Sampling effect; Non-significant Interaction",
       y = "Mean Abundance Change") +
  theme_classic()

#look at effect of grazing on scallop abundance---------------------------------

ggplot(comd %>% filter(taxa == "Argopecten.irradians", sampling %in% c("s2", "s5")), 
       aes(x = graze, y = pchange, fill = graze)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("gray", "seagreen")) +
  labs(title = "Effect of Grazing on Scallop Abundance",
       subtitle = "ANOVA p = 0.097",
       x = "Treatment",
       y = "Abundance Change") +
  theme_light() +
  theme(legend.position = "none")

# look at change in species evenness------------------------------------------
par(mfrow=c(2,2))
even<-lm(even~scar+pchangesd+sampling,data=comd%>%
          filter(taxa=="Argopecten.irradians")%>%
          filter(sampling %in% c("s2","s5")))

plot(even)
summary(even)
anova(even)

lp("ggeffects")
plot(ggeffect(even,terms=c("pchangesd")))

scallops<-aov(pchangeeven~graze*sampling,data=comd%>%
                filter(taxa=="Argopecten.irradians")%>%
                filter(sampling %in% c("s2","s5")))
par(mfrow=c(2,2))
plot(scallops)
summary(scallops)

TukeyHSD(scallops,which = "graze")

# look at msdens and scar and sampling
scallops2<-lm(pchangeeven~scar*pchangesd*sampling,data=comd%>%
                filter(taxa=="Argopecten.irradians")%>%
                filter(sampling %in% c("s2","s5")))

plot(scallops2)
anova(scallops2)
summary(scallops2)

plot(ggeffect(scallops2,terms=c("pchangesd","scar","sampling")))

#look at change in species abundance, percent cover-----------------------------
s1com<-com2%>%
  filter(sampling=="s1")%>%
  dplyr::select(plotID,taxa,preabund=abund,
                prempcover=mpcover,
                prespr=spr)

comd<-com2%>%
  filter(sampling!="s1")%>%
  left_join(s1com)%>%
  mutate(pchange=100*((abund-preabund)/(preabund)),
         pchange=ifelse(preabund==0,abund*100,pchange),
         pchangepc=100*((mpcover-prempcover)/(prempcover)),
         pchangespr=100*((spr-prespr)/(prespr)),
         pchangespr=ifelse(prespr==0,even*100,pchangespr))

# look at mpcover and scar and sampling
scallops2<-lm(pchange~scar*pchangepc*sampling,data=comd%>%
                filter(taxa=="Argopecten.irradians")%>%
                filter(sampling %in% c("s2","s5")))

plot(scallops2)
anova(scallops2)
summary(scallops2)

plot(ggeffect(scallops2,terms=c("pchangepc","scar","sampling")))
