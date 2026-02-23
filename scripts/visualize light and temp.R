# organize light and temperature data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")

theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           axis.title = element_text(size=18),
                           axis.text = element_text(size=14)))


# bring in logger data
byplots<-read.csv("wdata/loggerdata summarized by plot and sampling.csv")
byblocks<-read.csv("wdata/loggerdata summarized by block and sampling.csv")
initial<-read.csv("wdata/loggerdata summarized by plotID and pre or post grazing between may and june.csv")
sg<-read.csv("wdata/change in seagrass pc and sd.csv")

# look at how change in grazed and ungrazed plots from pre to post sampling
init2<-initial%>%
  group_by(blockID,prepost,graze.treat)%>%
  summarize(m.light=mean(m.light),
            sd.light=sd(m.light),
            m.temp=mean(m.temp),
            sd.temp=sd(m.temp))
init2$prepost<-factor(init2$prepost,levels = c("pre","post"))

initial$prepost<-factor(initial$prepost,levels = c("pre","post"))

initial<-left_join(initial,filter(sg,sampling=="s2")%>%select(-bay)%>%filter(taxa=="T"))

ggplot(data = initial,
       aes(x=prepost,y=m.light,color=pchange.sd,group=plotID))+
  geom_line()+
  geom_point()+
  facet_grid(bay~graze.treat,scales="free")+
  scale_color_viridis_c(option = "turbo")

# looking at it this way it doesn't seem like light had much of a role?

ggplot(data=initial%>%
         filter(prepost=="post")%>%
         filter(scar.treat=="U"),
       aes(y=pchange.sd,
           x=m.light,
           color=graze.treat))+
  geom_hline(aes(yintercept=0))+
  geom_point()+
  geom_smooth(method="lm")

summary(lm(pchange.sd~m.light*graze.treat,data=initial%>%filter(prepost=="post")%>%
             filter(scar.treat=="U")))

ggplot(data=initial%>%
         filter(prepost=="post")%>%
         filter(scar.treat=="U"),
       aes(y=pchange.sd,
           x=m.temp,
           color=graze.treat))+
  geom_hline(aes(yintercept=0))+
  geom_point()+
  geom_smooth(method="lm")

summary(lm(pchange.sd~graze.treat*m.temp,data=initial%>%filter(prepost=="post")%>%
             filter(scar.treat=="U")))
