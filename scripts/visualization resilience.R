# visualize resilience data

source("scripts/install_packages_function.R")

lp("tidyverse")
lp("vegan")

res.dat<-read.csv("wdata/resilience and sulfide.csv")

# set theme
theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           axis.text = element_text(size=14),
                           axis.title = element_text(size=16)))



bp<-ggplot(data=res.dat,aes(y=pcover,group=graze,color=graze))+
  geom_hline(aes(yintercept=0))+
  ylab("Percent change in seagrass % cover")+
  facet_wrap(~bay,scales="free")+
  scale_color_manual(values=c("#66c2a5","#fc8d62"),name="Grazing Treatment")+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"),name="Grazing Treatment")

  
bp+geom_point(size=3,aes(x=sulf.s4))+
  xlab("Sulfides in winter")


bp+geom_point(size=3,aes(x=sulf.s5))+
  xlab("Sulfides in summer")+
  geom_smooth(method="lm",aes(x=sulf.s5))

bp+geom_point(size=3,aes(x=PC))+
  xlab("Percent Carbon in Rhizomes")

bp+geom_point(size=3,aes(x=PN))+
  xlab("Percent Nitrogen in Rhizomes")+
  geom_smooth(method="lm",aes(x=PN))

bp+geom_point(size=3,aes(x=PS))+
  xlab("Percent Sulfide in Rhizomes")+
  geom_smooth(method="lm",aes(x=PS))

bp+geom_point(size=3,aes(x=CN))+
  xlab("Carbon:Nitrogen in Rhizomes")

bp+geom_point(size=3,aes(x=d13C))+
  xlab("d13 Carbon in Rhizomes")

bp+geom_point(size=3,aes(x=d15N))+
  xlab("d15N in Rhizomes")+
  geom_smooth(method="lm",aes(x=d15N))


bp+geom_point(size=3,aes(x=d34S))+
  xlab("d34S in Rhizomes")+
  geom_smooth(method="lm",aes(x=d34S))

ggplot(data=res.dat,aes(y=pcover))+geom_point(size=3,aes(x=d34S))+
  xlab("d34S in Rhizomes")+
  geom_smooth(method="lm",aes(x=d34S))

ggplot(data=res.dat,aes(y=canopy))+geom_point(size=3,aes(x=d34S))+
  xlab("d34S in Rhizomes")+
  geom_smooth(method="lm",aes(x=d34S))



bp+geom_point(size=3,aes(x=OMg.pergdw))+
  xlab("sediment OM")+
  geom_smooth(method="lm",aes(x=OMg.pergdw))

ggsave("figures/shootdensitychange and OM.jpg")

bp+geom_point(size=3,aes(x=dw.wt))+
  xlab("sediment dry weight: wet weight")+
  geom_smooth(method="lm",aes(x=dw.wt))

bp+geom_point(size=3,aes(x=epi.bm))+
  xlab("epiphyte biomass")

bp+geom_point(size=3,aes(x=above.ground))+
  xlab("above ground biomass")

bp+geom_point(size=3,aes(x=below.ground))+
  xlab("below ground biomass")

bp+geom_point(size=3,aes(x=above.below))+
  xlab("above:below biomass")+
  geom_smooth(method="lm",aes(x=above.below))

pairs(res.dat[,c(-1:-9,-25)])

#potential crappy model

densslm<-lm(pcover~OMg.pergdw*graze,data=res.dat)
par(mfrow=c(2,2))
summary(densslm)
anova(densslm)

# actually not so crappy. Has an adjusted r2 of 0.89 
lp("ggeffects")

pcov.mod<-ggpredict(densslm,terms=c("d34S","OMg.pergdw","graze"))
plot(pcov.mod)

# try a multivariate approach - first a pca

res.dat$graze.pred<-ifelse(res.dat$graze=="Graze",1,0)

res.dat2<-res.dat%>%
  drop_na()

sg.mresp<-res.dat2[,7:9]

# Create a full RDA model
full_rda_model <- rda(sg.mresp ~ ., data = res.dat2[,-1:-9])

# Create a null RDA model (only intercept)
null_rda_model <- rda(sg.mresp ~ 1, data = res.dat2[,-1:-9])

# Perform forward selection
selected_rda_model <- ordiR2step(null_rda_model, scope = formula(full_rda_model), direction = "backward")

# View the summary of the selected model
summary(selected_rda_model)      

res.dat2<-bind_cols(res.dat2,data.frame(scores(sg.pca,"sites",choices = c(1,2))))

sg.res<-data.frame(scores(sg.pca,"species",choices=c(1,2)))
sg.res2<-data.frame(scores(sg.pca,"bp",choices=c(1,2)))

ggplot(data=res.dat2)+#%>%
         #filter(bay=="SJ"))+
         #filter(graze=="Graze"))+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  geom_point(aes(x=RDA1,y=RDA2,color=graze,shape=bay),size=4)+
  scale_color_viridis_d(option="B",begin=.2,end=.8)+
  #geom_point(aes(x=RDA1,y=RDA2),shape=15,color="red",data=sg.res,size=3)+
  geom_text(aes(x=RDA1,y=RDA2,label = row.names(sg.res)),color="red",data=sg.res,size=3)+
  geom_text(aes(x=RDA1,y=RDA2,label = row.names(sg.res2)),color="black",data=sg.res2,size=3)+
  coord_equal()
