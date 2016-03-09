"
Author: Joe Tuccillo

Purpose: For a user-specified hazard zone and data, performs a community-
  level vulnerability cluster analysis by variable domain (i.e. social,
  economic, biophysical)

Inputs: User input specifying the hazard zone ([*zone*]) on which
  to perform community cluster analysis. This input references a 
  CSV file stored in the user's working directory. 
  
  User input is formatted as '[*zone*]_data.csv', such that entering
    'cat1' represents data for the category 1 storm surge zone 
    ('cat1_data.csv').

Outputs: A series of maps and average profile plots summarizing community 
  vulnerability characteristics for each domain (pdf format).

"

library(maptools)
library(fpc)
library(rgeos)
library(ggplot2)
library(stringr)

source("support_functions.R")

#User-specified hazard category
#for the purposes of keeping things abstract, can modify...
hcat<-readline("Enter a Hazard Category: ")

####Data Prep####

hazzn<-read.csv(paste0(hcat,"_data.csv"),stringsAsFactors = F)
data.dict<-read.csv("sandy_GIS_vars.csv",stringsAsFactors = F)
communities<-readShapePoly("communities_wgs.shp")

#fix fields with '%' 
#we should just fix this in the source data...
for(i in names(hazzn)[-c(1:2)]){
  
  if(is.character(hazzn[,i])){
    
    hazzn[,i]<-str_replace_all(hazzn[,i],"%","")
    hazzn[,i]<-str_replace_all(hazzn[,i],",","")
    hazzn[,i]<-as.numeric(hazzn[,i])
    
  }
  
}


####Clustering####

##Iterate through domains of interest, perform cluster analysis, 
#and generate plot objects.

for (dm in unique(data.dict[,1])){
  
  #Get variables for domain
  dm.sub<-data.frame(Community=as.character(hazzn[,2]),
                hazzn[,names(hazzn) %in% data.dict[data.dict[,1]==dm,]$Variable],
                stringsAsFactors = F)
  
  #get human-readable names for input vars
  inputnm<-data.dict[match(names(dm.sub)[-1],data.dict$Variable),]$Variable.Description
  
  #check for and remove unpopulated fields
  exclude0<-c()
  for (vv in names(dm.sub)[-1]){
    
    if(sum(hazzn[,vv])==0){
      
      exclude0<-c(exclude0,
                  which(names(dm.sub)==vv))
      
    }
    
  }
  
  cat(paste("For",toupper(dm),"excluding unpopulated fields:\n"),inputnm[exclude0-1],sep='\n')

  #exclude unpopulated fields
  inputnm<-inputnm[-(exclude0-1)] 
  
  #Wrap variable names to fit average profile plot 
  for(n in 1:length(inputnm)){ inputnm[n]<-paste(strwrap(inputnm[n],width = 30),collapse="\n") }
  
  #exclude unpopulated fields from 'dm.sub'
  dm.sub<-dm.sub[,-exclude0]
  
  #standardize input variables 
  dm.sub.z<-scale(dm.sub[,-1])
  
  #compute dissimilarity matrix (euclidian distance)
  dm.diss<-dist(dm.sub.z)
  
  #perform cluster analysis
  assign(paste0("CLUST_",dm),
         data.frame(Community=dm.sub$Community,
                    cluster=pamk(dm.diss,diss=T,krange=2:12)[1][[1]]$clustering,
                    stringsAsFactors = F))
  
  #generate map
  assign(paste0("MAP_",dm),
         plotMap(communities,get(paste0("CLUST_",dm)),
                 "NAMELSAD10","Community",custom.colors=brewer.pal(12,"Set3"),
                 map.title=paste0(dm,": ",hcat),return.map=T))
  
  #generate avg profile heatmap inputs
  dm.heat<-data.frame(get(paste0("CLUST_",dm)),dm.sub[,-1],stringsAsFactors = F)
  names(dm.heat)[-c(1:2)]<-inputnm
  assign(paste0("HEAT_",dm),dm.heat)
  
  cat(sep='\n\n')
}

####Generate average profile plots####

#Create a 'results' folder in working directory if none exists
if(!"results" %in% list.files()){ dir.create("results") }

m<-matrix(c(1,2))

for(dm in unique(data.dict[,1])){
  
  pdf(file=paste0("results/",hcat,"_",dm,".pdf"),width=20,height=16)
  multiplot(get(paste0("MAP_",dm)),
            plotHeat(get(paste0("HEAT_",dm)),return.heatMap = T),
            layout=m)
  
}