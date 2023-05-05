# This script makes a function that bring in datasets from google drive

# DO NOT PUSH THIS CODE TO GITHUB - IT EFFECTIVELY MAKES OUR DATA OPEN ACCESS AND 
# BOTH VIOLATES OUR MATS AND GIVE PEOPLE EDIT ACCESS TO OUR DATA.

# Stephanie K. Archer 1/6/2021
google.data<-function(url,filename,date.yn){
  #load packages
  source("scripts/install_packages_function.R")# this script load
  # the function that will install (if necessary) then load a package
  # in order to pull data from google drive you need the gsheet package
  lp("gsheet")
  
  #before you can use this code the sharing setting for the google sheet needs to be
  # set to "Anyone with the link can View"
  
  sheet.url<-url
  # each tab will have a slightly different URL make sure you copy the link 
  # for the tab you want to download
  temp1<-read.csv(text=gsheet2text(sheet.url,format='csv'),
                  stringsAsFactors = FALSE) # this downloads the data and saves it as 
  #as the object temp1
  
  #now save the data below are examples with and without adding the date to the file name
  
  #with the date added
 if(date.yn=="y")write.csv(x=temp1,file=paste0("odata/",filename,format(Sys.time(), '_%d_%B_%Y'),".csv"),row.names = FALSE)
  
  #without the date added
 if(date.yn=="n") write.csv(x=temp1,file=paste0("odata/",filename,".csv"),row.names = FALSE)
}

