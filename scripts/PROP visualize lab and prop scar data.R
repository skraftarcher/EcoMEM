# script to visualize field seagrass data and prop scar info

# load custom script to script to check if a package is installed, install it if necessary, then load it
source("scripts/install_packages_function.R")

# load the packages we need for this script
lp("tidyverse")

# load the data set
sg.prop<-read.csv("wdata/PROP_sg_lab_combined.csv")

# make some plots!

theme_set(theme_bw()+theme(panel.grid = element_blank()))# this sets some universal options
# the theme_bw removes the grey coloration from the background and puts a border around the plot
# the panel.grid = element_blank() removes that grid from the background. by using theme_set these options
# will be applied to every ggplot figure we make in this script.

# prop scar numbers ---- 
# (adding 4 - after the text above makes it into a header that I can use to navigate by later if this 
# script gets too long. you should see a drop down menu at the base of this window )

#ggplot graphs are built layer by layer - that means plots that will share the same
# base can all be build off of a saved object - reducing the amount of time we need to write
# up the same code
psn.100<-ggplot(data=sg.prop,aes(x=scar.quantity.100m,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.100+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))


# 50 m radius count----
psn.50<-ggplot(data=sg.prop,aes(x=scar.quantity.50m,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.50+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))


# 10 m radius count----
psn.10<-ggplot(data=sg.prop,aes(x=scar.quantity.10m,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.10+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))

# ave dist----
psn.dist<-ggplot(data=sg.prop,aes(x=ave.distance,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.dist+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))

# full scar length----
psn.full<-ggplot(data=sg.prop,aes(x=scar.length.full,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.full+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))

# scar length 100m----
psn.length100<-ggplot(data=sg.prop,aes(x=scar.length.clipped.100m,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.length100+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))

# scar length 50m----
psn.length50<-ggplot(data=sg.prop,aes(x=scar.length.clipped.50m,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.length50+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))

# scar length 10m----
psn.length10<-ggplot(data=sg.prop,aes(x=scar.length.clipped.10m,color=bay))# this code says I want to make a ggplot object using the data set sg.prop
# I want to set some of the aesthetics (the aes abbreviation here) - the x variable will be the number of scars in the 100m buffer, and I want to color my
# data points by bay

#now we can use this base to look at the relationship between the scar number at 100m and different seagrass
# metrics
psn.length10+
  # geom_point(aes(y=Tt.biom))
  # geom_point(aes(y=biom.per.day))
  # geom_point(aes(y=max.length))
  # geom_point(aes(y=PN))
  # geom_point(aes(y=PC))
  # geom_point(aes(y=PP))
  geom_point(aes(y=PC/PN))


