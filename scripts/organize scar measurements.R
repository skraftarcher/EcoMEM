# organize the scar data

# load datasets and packages
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("vegan")
lp("readxl")

pinfo<-read.csv("odata/allplotinfo.csv")%>%
  select(blockID,bay)%>%
  distinct()
scar.l<-read_xlsx("odata/downloaded_2026-01-05_EXPERIMENT - Prop Scar.xlsx",sheet = "Prop Scar- Block")
scar.w<-read_xlsx("odata/downloaded_2026-01-05_EXPERIMENT - Prop Scar.xlsx",sheet = "Prop Scar- Plot")
scar.sd<-read_xlsx("odata/downloaded_2026-01-05_EXPERIMENT - Prop Scar.xlsx",sheet = "Prop Scar Shoots")
samplings<-read_xlsx(paste0("odata/downloaded_2026-01-05_EXPERIMENT - Sampling Dates.xlsx"),sheet=1)


# organize data to have 1 length and average width and average shoot density per sg in the block
scar.l2<-scar.l%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  select(sampling,blockID,total.length)

scar.w2<-scar.w%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  group_by(blockID,sampling)%>%
  summarize(m.width=mean(width,na.rm=T),
            m.depth=mean(depth,na.rm=T))

scar.sd2<-scar.sd%>%
  mutate(sampling=case_when(
    date >= samplings$Start.date[1] & date <=samplings$End.date[1]~samplings$sampling[1],
    date >= samplings$Start.date[2] & date <=samplings$End.date[2]~samplings$sampling[2],
    date >= samplings$Start.date[3] & date <=samplings$End.date[3]~samplings$sampling[3],
    date >= samplings$Start.date[4] & date <=samplings$End.date[4]~samplings$sampling[4],
    date >= samplings$Start.date[5] & date <=samplings$End.date[5]~samplings$sampling[5]))%>%
  group_by(blockID,sampling,taxa)%>%
  summarize(m.sd=mean(no.shoots,na.rm=T))%>%
  filter(!is.na(taxa))%>%
  filter(taxa!="-")%>%
  pivot_wider(names_from=taxa,values_from=m.sd,values_fill = 0)

scar.full<-left_join(scar.l2,scar.w2)%>%
  left_join(scar.sd2)

write.csv(scar.full,"wdata/full_scar_byblock.csv",row.names = F)

scar.s1<-filter(scar.full,sampling=="s1")%>%
  rename("s1.l"=total.length,
         "s1.w"=m.width,
         "s1.d"=m.depth,
         "s1.t"='T',
         "s1.h"=H,
         "s1.s"=S)%>%
  select(-sampling)

scar.pchange<-right_join(pinfo,scar.full)%>%
  filter(sampling!="s1")%>%
  left_join(scar.s1)%>%
  mutate(l.pchange=(total.length-s1.l)/s1.l,
         w.pchange=(m.width-s1.w)/s1.w,
         d.pchange=(m.depth-s1.d)/s1.d,
         t.pchange=(`T`-s1.t)/s1.t,
         s.pchange=(S-s1.s)/s1.s,
         h.pchange=(H-s1.h)/s1.h)


write.csv(scar.pchange,"wdata/pchange_scar_byblock.csv",row.names = F)
