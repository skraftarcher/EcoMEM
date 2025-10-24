# this script is summarize seagrass data from surveys

# Stephanie K. Archer 09/20/2023


# load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("kableExtra")
lp("matrixStats")
lp("vegan")

#bring in data
source("scripts/download_propeller_scar_survey-EX.R")
props<-propscar%>%
  separate(siteID,
           into = c("bay", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove=FALSE)

props<-props[,!colnames(props) %in% c("X","X.1","X.2")]

props.oth<-filter(props,is.na(num))%>%
  separate(siteID,
           into = c("num","bay"),
           sep = "(?<=[0-9])(?=[A-Za-z])",
           remove=FALSE)%>%
  mutate(bay=ifelse(is.na(bay),"SJB",bay))

props<-props%>%
  filter(!is.na(num))%>%
  bind_rows(props.oth)


props.lengths<-props%>%
  mutate(g20.length=ifelse(is.na(as.numeric(length.shore))|is.na(as.numeric(length.water)),"yes","no"),
         tot.length=ifelse(g20.length=="no",
                           as.numeric(length.shore)+as.numeric(length.water),
                           20.1),
         length.cat=case_when(
           tot.length <=0 ~"0 m",
           tot.length >0 & tot.length<=5 ~ "1-5 m",
           tot.length > 5 & tot.length <=10~"6-10 m",
           tot.length > 10 & tot.length <=15~"11-15 m",
           tot.length > 15 & tot.length <=20~"16-20 m",
           tot.length >20 ~"+20 m"))%>%
  group_by(bay,length.cat)%>%
  summarize(nscars=n())%>%
  pivot_wider(names_from=length.cat,values_from=nscars,values_fill=0)%>%
  pivot_longer(2:6,names_to="length.cat",values_to="nscars")%>%
  mutate(bay=case_when(
    bay=="NB"~"North Bay",
    bay=="SAB"~"St. Andrew Bay",
    bay=="SJB"~"St. Joe Bay",
    bay=="WB"~"West Bay"))%>%
  filter(length.cat!="0 m")

props.lengths$length.cat<-factor(props.lengths$length.cat,levels=c("1-5 m","6-10 m","11-15 m","16-20 m","+20 m"))
props.lengths$bay<-factor(props.lengths$bay,levels=c("North Bay","West Bay","St. Andrew Bay","St. Joe Bay"))
theme_set(theme_bw()+theme(panel.grid=element_blank()))

bay.colors<-data.frame(bay=c("North Bay","West Bay","St. Andrew Bay","St. Joe Bay"),
                       colrs=c("#99d8c9","#41ae76","#005824","#d8b365"))


ggplot(data=props.lengths)+
  geom_bar(aes(x=length.cat,y=nscars,fill=bay),position=position_dodge(),stat = "identity",color="black")+
  xlab("Length of propeller scars")+
  ylab("Number of scars")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=14))+
  scale_fill_manual(values=bay.colors[,2],name="")

ggsave("figures/propeller_scars_by_bay.jpg",dpi=400,height=5,width=7)



