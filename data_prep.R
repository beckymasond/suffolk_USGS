"
Author: Joe Tuccillo

Description: Assembles raw ACS and NLCD input data by source dataset into an R dataframe
  of input variables for cluster analysis. 

Inputs: 

  1. 'data/blockgroup_all_combined_ests.csv': Combined ACS 5-Year Estimates (2014) at the
    block-group level for selected datasets..

  2. 'data/data_dict_ACS5Y2014.csv': data dictionary for selected ACS 
    datasets.

  3. 'data/NLCD_Suffolk_Grouped.RData': NLCD 2011 categories grouped for Suffolk County by 
    blockgroup. These are generated from 'landcover_prep.R' (NOT RUN as the NLCD files are 
    very large). 'landcover_prep.R' is included in this repository for reference (includes
    NLCD 2011 download). 

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

inVars<-read.csv("data/blockgroup_all_combined_ests.csv",stringsAsFactors = F)
data.dict<-read.csv("data/data_dict_ACS5Y2014.csv",stringsAsFactors = F)

#Exclude SE's (for now)
inVars<-inVars[,!grepl("SE_",names(inVars))]

#Make variable names human-readable using data dictionary
names(inVars)[-1]<-substr(names(inVars)[-1],1,(nchar(names(inVars)[-1])-1)) #remove trailing character from var names
names(inVars)[2:length(inVars)]<-sapply(X=names(inVars)[2:length(inVars)],FUN=function(X){ X<-data.dict[data.dict$Variable==X,]$Name })

#more readable reference for subsetting...
enumNames<-function(x){ data.frame(names(x)) } 

####Combine Variables####

##Create list for storing raw variable counts/totals (data frames).
#this is used to perform Gini diagnostics for clusters.
inVars.raw<-list()

##Create data frame for storing proportions of raw variables. 
inVars.prop<-data.frame(GEOID=inVars$GEOID)

#for converting raw data values to proportions
#now in 'support_functions.R'
# toprop<-function(inData){ return(data.frame(GEOID=inData$GEOID,sapply(X=inData[,-c(1:2)],FUN=function(X){X/inData$TOTAL}))) }

##I'm calculating totals by summing values for each dataset, since
#total fields appear to be missing. I'm assuming complete datasets were pulled (edit: not race/eth - need fix).
#I'll replace these with the total ests once I can find them to avoid
#any inconsistencies...

###Biophysical###

##Land Cover

#Load NLCD 2011 categories grouped for Suffolk County by blockgroup
# source("landcover_prep.R") #tabulate and group NLCD categories (not run)
load("data/NLCD_Suffolk_Grouped.RData")
inVars.raw$lc<-nlcd.tab #create list entry for 'inVars.raw'
inVars.prop<-merge(inVars.prop,toprop(nlcd.tab),by="GEOID") #append to 'inVars.prop'

##Housing Stock Age
inVars.raw$hage<-data.frame(GEOID=inVars$GEOID,TOTAL=inVars[,2]) #create list entry for 'inVars.raw'
inVars.raw$hage$HS.1940.before<-inVars[,3] #1940 or before
inVars.raw$hage$HS.Mid20th<-rowSums(inVars[,4:6],na.rm=T) #Mid-20th Century (1940-1970)
inVars.raw$hage$HS.Late20th<-rowSums(inVars[,7:9],na.rm=T) #Late 20th Century (1970-2000)
inVars.raw$hage$HS.2000s<-rowSums(inVars[,10:11],na.rm=T) #Since 2000
inVars.prop<-merge(inVars.prop,toprop(inVars.raw$hage),by="GEOID") #append to 'inVars.prop'

##Housing Stock Size
inVars.raw$hsize<-data.frame(GEOID=inVars$GEOID,TOTAL=inVars[,12])
inVars.raw$hsize$HU.SFR<-rowSums(inVars[,13:14],na.rm = T)
inVars.raw$hsize$HU.MFR.sm<-rowSums(inVars[,15:17],na.rm=T)
inVars.raw$hsize$HU.MFR.lg<-rowSums(inVars[,18:20],na.rm=T)
inVars.prop<-merge(inVars.prop,toprop(inVars.raw$hsize),by="GEOID")

##Home Values
inVars.raw$hval<-data.frame(GEOID=inVars$GEOID,TOTAL=inVars[,46])
inVars.raw$hval$HV.Under200k<-rowSums(inVars[,47:63],na.rm=T)
inVars.raw$hval$HV.200k.500k<-rowSums(inVars[,64:67],na.rm=T)
inVars.raw$hval$HV.500k.greater<-rowSums(inVars[,68:70],na.rm=T)
inVars.prop<-merge(inVars.prop,toprop(inVars.raw$hval),by="GEOID")


###SES###

##Race/Ethnicity
#**Is this the right total population field??**
inVars.raw$race.eth<-data.frame(GEOID=inVars$GEOID,TOTAL=inVars[,21],
                          inVars[,22:25])
inVars.prop<-merge(inVars.prop,toprop(inVars.raw$race.eth),by="GEOID")

##Age 
##ADD ME!!

##Income level
##ADD ME!!

##Family Structure
inVars.raw$fam<-data.frame(GEOID=inVars$GEOID,TOTAL=inVars[,26])
inVars.raw$fam$HH.Married<-inVars[,27]
inVars.raw$fam$HH.SinglePt<-inVars[,28]
inVars.raw$fam$HH.NonFamily<-rowSums(inVars[,29:30],na.rm=T)
inVars.prop<-merge(inVars.prop,toprop(inVars.raw$fam),by="GEOID")

##Length in Residence
##ADD ME!!

##Seasonal Homes
inVars.raw$seasonal<-data.frame(GEOID=inVars$GEOID,TOTAL=inVars[,44],Seasonal.Homes=inVars[,45])
inVars.prop<-merge(inVars.prop,toprop(inVars.raw$seasonal),by="GEOID")

####Cleanup#####

###Remove observations with household count of 0###

##Generate empty GEOIDs list
empty.list<-list.files(paste0(getwd(),"/data"))
empty.list<-empty.list[grepl("_empty",empty.list)]

empties<-data.frame()
for (empty in empty.list){
  
  emp<-read.csv(paste0("data/",empty),header=F,stringsAsFactors = F)
  empties<-rbind(empties,emp)
  
}

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

