"
Author: Joe Tuccillo

Description: Tabulates and Groups NLCD 2011 Data for Suffolk County, 
  block groups. 

Inputs: 

  1. 'data/tl_2014_36_bg.shp': NY State block group boundaries.

  2. National Land Cover Database (NLCD) 2011 Land Cover Raster. 
    (Downloaded to 'data' in script).

  3. 'data/nlcd_key_simple.csv': NLCD land cover data dictionary 
    by pixel value ('Desc'). Also contains a grouping scheme for values 
    tabulated for Suffolk County ('Group') 

Outputs: RData file of pixel counts by block group for combined 
    NLCD categories ('NLCD_Suffolk_Grouped.RData'). 

"

library(raster)
library(rgdal)
library(maptools)

##Load Data
#Suffolk County blockgroups 
sufbg<-readOGR(dsn="data",layer = "tl_2014_36_bg")
sufbg<-sufbg[sufbg$COUNTYFP=="103",]
sufbg$GEOID<-paste0("g",sufbg$GEOID)

##Download and load NLCD 2011 into R Environment
#(might need to change the download url, .img filename if updated)
if(!"download" %in% list.files("data")){ dir.create("data/download") } #create a download folder if none exists
download.file(url="http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip","data/download/nlcd11.zip")
unzip("data/download/nlcd11.zip",exdir="data")
nlcd<-raster("data/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img") #load data

##Extract NLCD values
sufbg<-spTransform(sufbg,CRSobj=crs(nlcd)) #transform CRS of blockgroups to match nlcd
suflc<-crop(nlcd,sufbg) #crop nlcd by blockgroups
suflc.extract<-extract(suflc,sufbg) #get all nlcd pixel values within blockgroups

##Tabulate NLCD values 
rtab<-data.frame(GEOID=sufbg$GEOID)

for (i in 1:length(suflc.extract)){
  
  unit.ext<-suflc.extract[i][[1]] #Get unit raster extraction 
  px.values<-unique(unit.ext)[!is.na(unique(unit.ext)) & unique(unit.ext)!=0] #Get unique px values for unit
  
  #Iterate through unique pixel value categories within blockgroup
  #and append them to 'rtab' for the blockgroup.
  for (px in px.values){
    
    pxct<-length(unit.ext[unit.ext==px]) #count of pixels of type 'px' in area
    
    if (paste0("tab.",px) %in% names(rtab)){ #Tab field is already present, append value 
      
      rtab[i,][[paste0("tab.",px)]]<-pxct
      
    }else{ #Add tab field if not already present in 'rtab'
      
      #Assume that all preceding units have none of the px values, 
      #and all following units are 0 (will overwrite with iteration)
      rtab[[paste0("tab.",px)]]<-c(rep(0,i-1),pxct,rep(0,length((i+1):length(suflc.extract))))
      
    }
    
  }
  
}

##Replace tab fields with human-readable names
nlcd.key<-read.csv("data/nlcd_key_simple.csv",stringsAsFactors=F)
names(rtab)[-1]<-sapply(names(rtab)[-1],
                        FUN=function(X){X<-nlcd.key[nlcd.key$Value==substr(X,5,nchar(X)),]$Desc})

##Group NLCD categories by custom schema ('Group' field in 'nlcd.key') and assign to output dataframe
lcgrp<-unique(nlcd.key$Group[nlcd.key$Group!=999])
nlcd.tab<-data.frame(GEOID=rtab$GEOID,TOTAL=rowSums(rtab[,-1])) #output nlcd tabulation data frame 

#append combined fields to 'nlcd.tab'
for(g in lcgrp){
  
  gmemb<-names(rtab) %in% nlcd.key[nlcd.key$Group==g,]$Desc
    
  if (sum(gmemb)>1){
    
    nlcd.tab[[g]]<-rowSums(rtab[,gmemb])
    
  }else{
    
    nlcd.tab[[g]]<-rtab[,gmemb]
    
  }

}

##Save Data
save(nlcd.tab,file="data/NLCD_Suffolk_Grouped.RData")
