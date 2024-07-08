# script to visualize field seagrass data and prop scar info

# load custom script to script to check if a package is installed, install it if necessary, then load it
source("scripts/install_packages_function.R")

# load the packages we need for this script
lp("tidyverse")

# load the data set
sg.prop<-read.csv("wdata/PROP_combined_sg_prop.csv")

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

# coiunts in 100m radius----
psn.100+
  geom_point(aes(y=pc.m_H)) # you can look at each of these graphs by removing the hashtag from before the line - just make sure to only look at one line at a time
  # geom_point(aes(y=pc.m_S))
  # geom_point(aes(y=pc.m_Tt))
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_S))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_S))
  # geom_point(aes(y=sht.d.m_Tt)) # to me it looks like an odd relationship in SAB where thalassia is more abundant where there are more scars...
# might need to think this through - but it looks like its there in shoot densities and % cover
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_S))
  # geom_point(aes(y=sht.d.sd_Tt))
# besides the comment I already made what these tell me is that we did not see enough syringodium
# to bother plotting it further (already knew this was the case with He)
  # geom_point(aes(y=sg.rich))
  # geom_point(aes(y=sg.even))# don't bother plotting again - not enough there
  # geom_point(aes(y=alg.rich)) # don't bother plotting again - not enough there
  # geom_point(aes(y=alg.even))# don't bother plotting again - not enough there
  # geom_point(aes(y=can.ht.m))
  

#counts in 50m radius---- 
psn.50<-ggplot(data=sg.prop,aes(x=scar.quantity.50m,color=bay))

psn.50+
  # geom_point(aes(y=pc.m_H))
  # geom_point(aes(y=pc.m_Tt))
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_Tt)) # that relationship is noisier but might still be there...
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_Tt)) # shoot density looks to be more variable with increasing scar quantity
  # geom_point(aes(y=sg.rich))
  geom_point(aes(y=can.ht.m))

#counts in 10m radius----  
psn.10<-ggplot(data=sg.prop,aes(x=scar.quantity.10m,color=bay))

psn.10+
  # geom_point(aes(y=pc.m_H))
  # geom_point(aes(y=pc.m_Tt)) # that relationship is there again... 
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_Tt)) # that relationship is still there...
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_Tt)) # shoot density looks to be more variable with increasing scar quantity
  # geom_point(aes(y=sg.rich))
  geom_point(aes(y=can.ht.m))

#average distance between plots----  
avedis<-ggplot(data=sg.prop,aes(x=ave.distance,color=bay))

avedis+
  # geom_point(aes(y=pc.m_H))
  # geom_point(aes(y=pc.m_Tt))
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_Tt)) # looks like (messy) less clumped = more shoots
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_Tt))
  # geom_point(aes(y=sg.rich))
  geom_point(aes(y=can.ht.m))


#scar length - full ----  
sl.full<-ggplot(data=sg.prop,aes(x=scar.length.full,color=bay))

sl.full+
  # geom_point(aes(y=pc.m_H))
  # geom_point(aes(y=pc.m_Tt))
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_Tt)) # same relationship popping up here - more scars = more shoots??
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_Tt))
  # geom_point(aes(y=sg.rich))
  geom_point(aes(y=can.ht.m))

  
#scar length - 100m ----  
sl.100<-ggplot(data=sg.prop,aes(x=scar.length.clipped.100m,color=bay))

sl.100+
  # geom_point(aes(y=pc.m_H))
  # geom_point(aes(y=pc.m_Tt))
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_Tt)) # same relationship popping up here - more scars = more shoots??
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_Tt))
  # geom_point(aes(y=sg.rich))
  geom_point(aes(y=can.ht.m))

#scar length - 50m ----  
sl.50<-ggplot(data=sg.prop,aes(x=scar.length.clipped.50m,color=bay))

sl.50+
  # geom_point(aes(y=pc.m_H))
  # geom_point(aes(y=pc.m_Tt))
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_Tt)) # same relationship popping up here - more scars = more shoots??
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_Tt))
  # geom_point(aes(y=sg.rich))
  geom_point(aes(y=can.ht.m))


#scar length - 10m ----  
sl.10<-ggplot(data=sg.prop,aes(x=scar.length.clipped.10m,color=bay))

sl.10+
  # geom_point(aes(y=pc.m_H))
  # geom_point(aes(y=pc.m_Tt))
  # geom_point(aes(y=pc.sd_H))
  # geom_point(aes(y=pc.sd_Tt))
  # geom_point(aes(y=sht.d.m_H))
  # geom_point(aes(y=sht.d.m_Tt)) # same relationship popping up here - more scars = more shoots??
  # geom_point(aes(y=sht.d.sd_H))
  # geom_point(aes(y=sht.d.sd_Tt))
  # geom_point(aes(y=sg.rich))
  geom_point(aes(y=can.ht.m))
