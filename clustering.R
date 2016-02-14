##Load libraries & support functions
source("support_functions.R",echo=F)

##Generate and load input data
source("data_prep.R")
load("data/Suffolk_USGS_Inputs.RData")

##Load blockgroups
bgs<-readShapePoly("data/tl_2014_36_bg.shp")
bgs$GEOID<-paste0("g",bgs$GEOID) #add leading character for compatibility with inputs
bgs<-bgs[grepl("g36103",bgs$GEOID),] #subset by Suffolk County

##Load hazard zone
# hazzn<-readShapePoly("data/hazard_extent_Suffolk.shp")
hazzn<-readShapePoly("data/hazard_extent_Suffolk_wgs.shp")

####Clustering####

###Prep

##standardize (z-score) inputs
inVars.z<-data.frame(GEOID=inVars.prop$GEOID,scale(inVars.prop[,-1])) 

###Data Reduction

##PCA
sufPCA<-princomp(inVars.z[,-1])

##Subset PCA scores
##Using the Kaiser Criterion (retain PC's with eigenvalues >=0.1).
##Can change retention rule as needed...
varexpl<-(sufPCA$sdev)^2 / sum(sufPCA$sdev^2) #pct variance explained
kaiser<-length(which(varexpl>=0.1)) #length of PC's fulfilling kaiser criterion
sufScores<-sufPCA$scores[,1:kaiser] #get PC scores - retain KC components

###Cluster Analysis

##k-medoids (PAM) with optimal cluster selection (library 'fpc')
##let cluster choice float from k=7 to k=12 groups (can also change as needed).
##starting k is set high to differentiate among hazard zone bg's --
##tried a lower number, (krange=4:12, k=5) and group representation in 
##hazard zone was fairly homogeneous.
##This will probably change as we add more inputs - so I will keep adjusting.
sufClust<-data.frame(GEOID=inVars.z$GEOID,
                     cluster=pamk(sufScores,krange=7:12)[1][[1]]$clustering,
                     stringsAsFactors = F)


####Visualization and Diagnostics####

###Plot map of cluster results
overview.map<-plotMap(bgs,sufClust,"GEOID","GEOID",custom.colors=brewer.pal(12,"Set3"),
        map.title=paste0("Suffolk County Cluster Solution, k=",max(unique(sufClust$cluster))),
        return.map=T)
overview.map

# ###Cluster results in the hazard zone
#TO DO!! - something is funky with the hazard zone geometry - it isn't playing nice with blockgroups.
# haz.map<-overview.map<-plotMap(bgs[(!is.na((bgs %over% hazzn)$GEOID),],sufClust,"GEOID","GEOID",custom.colors=brewer.pal(12,"Set3"),
#                                map.title="Groups in the Hazard Zone",return.map=T)
# multiplot(overview.map,haz.map)


###Average Profile Heatmap
sufProfile<-merge(sufClust,inVars.prop)
plotHeat(sufProfile)

###Gini Diagnostics
#Population-weighted Gini index (Brown 1994) and average profiles are compared to 
#identify which variables with high/low avg profiles are relatively evenly distributed
#about each cluster group.

#Printout of Gini Profiles
sapply(X=unique(sufProfile$cluster),
       FUN=function(X){do.call(giniProfile,args=list(inData=sufProfile,gini.thresh=0.3,mean.thresh=0.1,var.raw=inVars.raw,cluster=X))})

#Bar Charts
##TO DO!!

###Cluster Groups by Community

#load communities
sufMunis<-readShapePoly("data/communities_wgs.shp")

#subset blockgroups by communities
bgs.in.muni<-bgs %over% sufMunis
bgs.in.muni<-cbind(bgs@data,bgs.in.muni)
bgs.in.muni<-bgs.in.muni[!is.na(bgs.in.muni$NAME10),]

#This should be total population **NEED TO VERIFY**
totalPops<-inVars.raw$race.eth[,c("GEOID","TOTAL")] 

#Merge pops & cluster assignments to bg.in.muni
bgs.in.muni<-merge(merge(totalPops,bgs.in.muni,by="GEOID"),sufClust,by="GEOID")

#Aggregate by community/cluster
clustPops<-with(bgs.in.muni,aggregate(TOTAL~NAME10+cluster,FUN=sum))
muniPops<-with(bgs.in.muni,aggregate(TOTAL~NAME10,FUN=sum))

##Readout of cluster percentages 
for (m in 1:nrow(muniPops)){
  
  muni<-muniPops[m,]$NAME10
  
  cat(paste0("Groups Representing ",muni,":"),sep="\n\n")
  
  clusts<-clustPops[clustPops$NAME10==muni,]
  
  for (cl in unique(clusts$cluster)){
    
    clshr<-round(100*(clusts[clusts$cluster==cl,]$TOTAL/muniPops[m,]$TOTAL),2)
    
    cat(paste0("CLUSTER ",cl,": ",clshr,"%"),sep="\n")
    
  }
  
  cat(sep="\n\n")
  
}
