"
Author: Joe Tuccillo

Description: Assembles raw ACS and NLCD input data by source dataset into an R dataframe
  of input variables for cluster analysis. 

Inputs: 

  1. 'data/rawACSdata_all/blockgroup_all_ests.csv': Combined ACS 5-Year Estimates (2014) at the
    block-group level for selected datasets..

  2. 'data/data_dict_ACS5Y2014.csv': data dictionary for selected ACS 
    datasets.

  3. 'master_vars.csv': ACS variable grouping scheme by variable names and codes.

  4. 'data/NLCD_Suffolk_Grouped.RData': NLCD 2011 categories grouped for Suffolk County by 
    blockgroup. These are generated from 'landcover_prep.R' (NOT RUN as the NLCD files are 
    very large). 'landcover_prep.R' is included in this repository for reference (includes
    NLCD 2011 download). 

  5. 'data/rawACSdata_all/blockgroup_all_empty.csv': list of unpopulated block groups by 
    block group ACS code.

Outputs: RData file 'Suffolk_USGS_Inputs.RData', which contains: 

  1. 'inVars.prop': a dataframe of input variables for cluster analysis,
    combined from ACS estimates. Variables are represented as proportions
    of the total populations of their source datasets. 
  
  2. 'inVars.raw': a supplementary list of dataframes sorted by source ACS dataset.
    Each dataframe features the total population of the source dataset and raw
    counts of variables comprising 'inVars.prop'. Used for clustering 
    diagnostics (i.e. Gini index).

"

####Setup####

inVars<-read.csv("data/rawACSdata_all/blockgroup_all_ests.csv",stringsAsFactors = F) 
data.dict<-read.csv("data/data_dict_ACS5Y2014.csv",stringsAsFactors = F)
group.key<-read.csv("master_vars.csv",stringsAsFactors = F)[,c("Category","Category.Code","Estimate","Universe.Variable")] #for grouping variables

##Make variable names human-readable using data dictionary
acs.idx<-which(names(inVars)!="GEOID" & !grepl("SE_",names(inVars))) #index of ACS var names
se.idx<-which(grepl("SE_",names(inVars))) #index of social explorer var names 
names(inVars)[acs.idx]<-substr(names(inVars)[acs.idx],1,(nchar(names(inVars)[-1])-1)) #remove trailing character from ACS var names
names(inVars)[se.idx]<-substr(names(inVars)[se.idx],4,nchar(names(inVars)[se.idx])) #remove prefix from SE var names
names(inVars)[-1]<-sapply(X=names(inVars)[-1],FUN=function(X){ X<-data.dict[data.dict$Variable==X,]$Name }) #update names

#Update 'group.key$Universe.Variable' (dataset total codes) for compatibility with 'data.dict'
for (v in unique(group.key$Universe.Variable)){
  
  #Get a second instance of the variable name for updating
  vnm<-group.key[group.key$Universe.Variable==v,]$Universe.Variable
  
  if(grepl("ACS14_5yr",v)){ #ACS Variable
    
    vnm=substr(vnm,11,nchar(vnm)) #strip leading ACS code
    vnm=paste(substr(vnm,1,(nchar(vnm)-3)),"001",sep="_") #update code suffix
  
  }else if(substr(v,1,1)==" "){ #Social Explorer Variable with leading indent problem
    
    #remove indent
    vnm=gsub(" ","",vnm)
    
  }

  group.key[group.key$Universe.Variable==v,]$Universe.Variable<-vnm #reassign var name
  
}

####Combine Variables####

##Create list for storing raw variable counts/totals (data frames).
#this is used to perform Gini diagnostics for clusters.
inVars.raw<-list()

##Create data frame for storing proportions of raw variables. 
inVars.prop<-data.frame(GEOID=inVars$GEOID)


##Prep Land Cover inputs

#Load NLCD 2011 categories grouped for Suffolk County by blockgroup
# source("landcover_prep.R") #tabulate and group NLCD categories (not run)
load("data/NLCD_Suffolk_Grouped.RData")
inVars.raw$lc<-nlcd.tab #create list entry for 'inVars.raw'
inVars.prop<-merge(inVars.prop,toprop(nlcd.tab),by="GEOID") #append to 'inVars.prop'


##Prep ACS Variables
for(vvar in unique(group.key$Category)){

  #Get category total. This is *really* inefficient, we should simplify... 
  vtot<-data.dict[data.dict$Variable==group.key[group.key$Category==vvar,]$Universe.Variable[1],]$Name
  
  print(paste(vvar,vtot))
  
  #Group variable counts 
  inVars.raw[[vvar]]<-data.frame(GEOID=inVars$GEOID,TOTAL=inVars[,vtot])
  
  #Get category variable codes
  catv<-group.key[group.key$Category==vvar,]
  
  #Assemble composite vars and assign to 'inVars.raw[[*category*]]'
  for(gvar in unique(catv$Category.Code)){
    
    #index values of variables for grouping in inVars
    #NOTE that if a variable is missing it will be ignored!!
    gvar.idx<-which(names(inVars) %in% catv[catv$Category.Code==gvar,]$Estimate)
    
    #group variables and append to category df
    if(length(gvar.idx)>1){
      
      inVars.raw[[vvar]][[gvar]]<-rowSums(inVars[,gvar.idx],na.rm=T)
      
    }else{
      
      inVars.raw[[vvar]][[gvar]]<-inVars[,gvar.idx]
      
    }
  }
  
  #convert to proportions and append to inVars.prop
  inVars.prop<-merge(inVars.prop,toprop(inVars.raw[[vvar]]),by="GEOID")
}


####Cleanup#####

###Remove observations with household count of 0###

empties<-read.csv("data/rawACSdata_all/blockgroup_all_empty.csv",stringsAsFactors=F)[,1]

##Remove empty observations from 'inVars.prop' and 'inVars.raw'
inVars.prop<-inVars.prop[!inVars.prop$GEOID %in% empties,]
inVars.raw<-sapply(X=inVars.raw,FUN=function(X){ X<-X[!X$GEOID %in% empties,] })

###Reset NaN values to 0 (these get generated when var=0 and TOTAL=0)
for(i in 2:ncol(inVars.prop)){ inVars.prop[,i][is.nan(inVars.prop[,i])]<-0 }

###Subset Data by Complete Cases###
inVars.prop<-inVars.prop[complete.cases(inVars.prop),]
inVars.raw<-sapply(X=inVars.raw,FUN=function(X){ X<-X[complete.cases(X),] })


####Save Data####
save(list = c("inVars.raw","inVars.prop"),file="data/Suffolk_USGS_Inputs.RData")

