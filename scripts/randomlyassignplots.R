# script to randomize grazing treatment placement

#Stephanie K. Archer 
#Nov. 16, 2023

plottreat<-data.frame(blockID=c(rep("SJ1",4),
                                rep("SJ2",4),
                                rep("SJ3",4),
                                rep("SJ4",4),
                                rep("SJ5",4),
                                rep("SJ6",4),
                                rep("SA1",4),
                                rep("SA2",4),
                                rep("SA3",4),
                                rep("SA4",4),
                                rep("SA5",4),
                                rep("SA6",4)),
                      Propscar=rep(c("S","S","U","U"),12))
grazed<-NA
grazed<-grazed[0]
for(i in 1:24){
  t1<-sample(c("G","U"),2,FALSE)
  grazed<-c(grazed,t1)
}

plottreat<-cbind(plottreat,grazed)

plottreat$plotID<-paste0(plottreat$blockID,plottreat$Propscar,plottreat$grazed)

write.csv(plottreat,"odata/plotassignments_nov162023.csv",row.names = FALSE)
