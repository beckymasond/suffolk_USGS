library(rgeos)
library(maptools)
library(RColorBrewer)
library(cluster)
library(ggplot2)
library(reshape2)
library(fpc)

toprop<-function(inData){ 
  
  return(data.frame(GEOID=inData$GEOID,sapply(X=inData[,-c(1:2)],FUN=function(X){X/inData$TOTAL}))) 
  
}


compareMv<-function(inData,verbose=F,return.means=F){
  
  "
  
  Get mean vector of variables of interest for a specified extent.
  
  "
  
  inData<-aggregate(x=inData[,3:length(inData)],by=list(inData[,2]),na.rm=T,FUN=mean)
  all.mean<-colMeans(inData[,2:ncol(inData)])
  
  if (verbose){
    cat(sep='\n\n')
    cat(paste("Variable Averages:",toString(names(all.mean))),all.mean,sep="\n")
  }
      
  if(return.means){ 
    
    return(all.mean) 
    
  }else{
  
    for (i in 2:ncol(inData)){inData[,i]<-round(100*(inData[,i]-all.mean[i-1])/all.mean[i-1])}
    
    return(inData)
  
  }  
    
}

plotHeat <- function(inData,flip=FALSE,transpose=FALSE,compare=FALSE,series.list=NULL,return.heatMap=F){
  
  "
  Plot average profile characteristics of classification/variables of interest in a
  'heatmap' style raster plot.
  
  Inputs: 
  
  inData (Dataframe): Input data - must be numeric and feature 'GEOID' (enum unit)
  and 'type' (cluster type) fields as first two columns
  
  Outputs: 
  
  Plots heatmap in R environment. 
  
  Options: 
  
  mv.reg: use smoothed region mean vector for comparison
  flip: reverse cardinality of values
  
  "
  
  econz<-inData
  
  econz<-compareMv(econz)
  
  econz$Group.1<-as.factor(econz$Group.1)
  econM=melt(econz)
  
  ##Flip value cardinality if specified
  #This is useful i.e. when profiling dissimilarity and similar values low/dissimilar values high
  if (flip){
    
    econM$value=-econM$value
    
  }
  
  if (compare){
    
    series<-c()
    for (v in unique(econM$variable)){
      
      vv<-series.list[series.list$variable==v,]$series
      series<-c(series,rep(vv,nrow(econM[econM$variable==v,])))
      
    }
    
    econM$series<-series
    
  }
  
  if (!transpose){
    
    heatMap<-ggplot(econM,aes(variable,Group.1,fill=value))+
      geom_raster()+
      scale_fill_gradient2(high='#e66101',mid='#f7f7f7',low='#5e3c99',guide="colourbar",na.value='gray90')+
      geom_text(aes(label=value, size=4),size=5,fontface='bold')+
      theme(axis.text.x = element_text(angle=45, hjust=1))+
      #       xlab("Attribute")+
      #       ylab("Class")+
      theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
      theme(legend.position="none")
    
    if(compare){
      
      heatMap<-heatMap+facet_grid(~series,scales="free",as.table=T)
      
    }
    
    plot(heatMap)
    
  }else{ #flip the values
    
    heatMap<-ggplot(econM,aes(Group.1,variable,fill=value))+
      geom_raster()+
      scale_fill_gradient2(high='#e66101',mid='#f7f7f7',low='#5e3c99',guide="colourbar",na.value='gray90')+
      geom_text(aes(label=value, size=4),size=5,fontface='bold')+
      theme(axis.text.x = element_text(angle=45, hjust=1))+
      #       xlab("Attribute")+
      #       ylab("Class")+
      theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
      theme(legend.position="none")
    
    plot(heatMap)
    
  }
  
  if(return.heatMap){return(heatMap$data[,2:3])}
}

plotMap<-function(inGeom,inClust,geomKey,clustKey,compare=FALSE,map.title=NULL,custom.colors=NULL,legend.key.extend=FALSE,
                  plot.borders=T,plot.basemap=F,base.geom=NULL,hide.gridlines=F,return.map=F){
  
  "
  Plots a map of a given cluster result(s). 
  
  Inputs:
  
  inGeom (SpatialPolygons Dataframe): Enumeration units on which cluster analysis is based  
  geomKey (Character): geometry id field for merge 
  inClust (Dataframe): cluster types by id (clustKey)
  should only consist of an id and cluster field (must be named 'cluster')
  clustKey (Character): cluster id field for merge
  compare: if TRUE, plots multiple maps as a facet wrap using a 'series' field
  map.title (Character): optional map title
  legend.key.extend (Logical): increase the height of the legend key items
  
  Output: 
  
  Plots map(s) in R environment. 
  
  "
  
  #Fortify inGeom
  map_df<-fortify(inGeom,region=geomKey)
  
  #merge the data
  map_df<-merge(inClust,map_df,by.x=clustKey,by.y="id")
  
  
  ##Identify holes for subset in plot
  #We need to convert from factor->character->factor to remove unecessary levels
  holes<-as.factor((map_df[map_df$hole==TRUE,][[clustKey]]))
  
  #Assign boolean to blocks with holes 
  map_df$has.hole<-map_df[[clustKey]] %in% holes
  
  #Order the map dataframe 
  # map_df<-map_df[order(map_df$variable,map_df$GEOID,map_df$order),]
  map_df<-map_df[order(map_df[[clustKey]],map_df$order),]
  
  #Make sure the "value" field is a factor
  map_df$cluster<-as.factor(map_df$cluster)
  
  #Generate map 
  map.out<-ggplot(aes(long,lat,group=group),data=map_df) + 
    geom_polygon(data=subset(map_df,has.hole==TRUE),aes(fill=cluster))+
    geom_polygon(data=subset(map_df,has.hole==FALSE),aes(fill=cluster))+
    coord_equal()+
    # geom_path(data=inGeom, colour = "gray40", size = .5,alpha=.2)+
    theme(legend.title=element_blank())
  
  #Plot borders
  if (plot.borders){
    
    map.out<-map.out+geom_path(data=inGeom, colour = "gray40", size = .5,alpha=.2)
    
  }
  
  if (plot.basemap){
    
    map.out<-map.out+geom_path(data=base.geom, colour = "gray40", size = .6)
    
  }
  
  #Hide gridlines (background)
  if (hide.gridlines){
    
    map.out<-map.out+theme(panel.background = element_blank())
    
  }
  
  
  #Color ramp
  if (is.null(custom.colors)){ #Default to 'Spectral' ColorBrewer ramp 
    
    map.out<-map.out+scale_fill_brewer(palette="Spectral")
    
  }else{
    
    map.out<-map.out+scale_fill_manual(values = custom.colors)+scale_colour_identity()
    
  }
  
  #Legend expansion
  if(legend.key.extend){
    
    map.out<-map.out+theme(legend.key.height = unit(0.5, "in"))
    
  }
  
  #Add facets, if specified
  if (compare==TRUE){
    map.out<-map.out+facet_wrap(~series)
  }
  
  #Add map title, if specified
  if (!is.null(map.title)){
    map.out<-map.out+ggtitle(map.title)
  }
  
  #   plot(map.out)
  if(return.map){
    
    return(map.out)
    
  }else{
    
    print(map.out) 
    
  }
  
}

evaluate_class<-function (data, cluster_lookup, groups_or_types, variables,denominator_name){
  options(warn = -1)
  df <- data.frame(data) #Write input data columns to df 
  
  b <- c() #Vector for storing numeric cluster id's
  clusters <- c() #Vector for storing cluster type or group id's
  
  #Check whether a valid "groups_or_types" field is specified
  if (!(groups_or_types == "types") & !(groups_or_types == 
                                        "groups")) {
    cat("Groups or types not specified!. This is required. Please try again.", 
        "\n")
    stop
  }
  
  #Get rid of the letter heading for cluster types 
  #and assign to new variable 'b'
  if (groups_or_types == "types") {
    clusters <- as.character(cluster_lookup[, 2])
    for (i in 1:length(clusters)) {
      clusters[i] <- gsub("[A-Z]", "", clusters[i])
    }
    b <- clusters #Assign clusters to b 
  }
  
  #Create representation 'b' for input groups 
  if (groups_or_types == "groups") {
    clusters <- as.character(cluster_lookup[, 2])
    for (i in 1:length(clusters)) {
      temp <- clusters[i]
      if (nchar(temp) == 1) {
        b[i] <- match(temp, LETTERS)
      }
      if (nchar(temp) > 1) {
        temp <- substr(temp, 1, 1)
        b[i] <- match(temp, LETTERS)
      }
    }
  }
  
  #Convert cluster types to numeric 
  b <- as.numeric(b)
  
  #Append 'b' to cluster types 
  df <- cbind(df, b)
  
  #Generate a matrix of cluster types 
  mat <- matrix(nrow = max(b), ncol = 0)
  
  #Get df column names 
  vec <- colnames(df)
  
  #Iterate through the list of variables 
  #And aggregate by cluster type 
  for (k in 1:(length(vec) - 1)) { 
    
    #Sum df columns by variable 
    d2 <- as.vector(by(df[, k], df$b, sum))
    mat <- cbind(mat, d2)
  }
  
  #The output 'mat' corresponds row-wise to cluster type ('b')
  #and column-wise to variables ('vec')
  #**Not sure what's being aggregated to each cell - row sums by cluster type?**
  
  #Get rid of df$b
  l <- length(vec) 
  df[, l] <- NULL
  
  #Number of unique values is the same as cluster number 
  num_clust <- max(b)
  
  #Reassign variable names to mat & convert to df
  colnames(mat) <- colnames(df)
  mat <- data.frame(mat)
  
  #mat represents aggregate donations by cluster type?
  
  #Create a list of index values corresponding to cluster number 
  Cluster_Number <- c()
  for (i in 1:num_clust) {
    Cluster_Number[i] <- i
  }
  
  master_list <- list() #Generate an empty list for storing Gini Coefficients by variable
  mat <- cbind(Cluster_Number, mat) #Assign cluster categories back to mat (these were previously just row indices) 
  
  #   View(mat)
  
  #Iterate through the list of variables 
  
  for (k in 1:length(variables)) {
    
    variable_name <- variables[k] #Assign current variable name 
    
    names <- colnames(data) #Get input data column names 
    
    num1 <- match(variable_name, names) #Get the index value of the current variable name 
    
    denominator<-denominator_name #Get current denominator name 
    
    num2 <- match(denominator, names) #Get index value of denominator 
    
    #Get aggregate numerator and denominator columns from the list 
    denominator <- mat[, num2 + 1] #Total population field
    numerator <- mat[, num1 + 1] #Variable of interest 
    
    #Get the list of cluster numbers 
    cluster_number <- mat$Cluster_Number
    
    #Generate matrix of denominators and numerators 
    #Fore each cluster type 
    nmat <- matrix(nrow = num_clust, ncol = 0)
    nmat <- cbind(cluster_number, denominator)
    nmat <- cbind(nmat, numerator)
    
    denom <- c()
    numer <- c()
    
    for (i in 1:num_clust) {
      denom[i] <- (nmat[i, 2]/sum(nmat[, 2])) #Proportion of tract population to total population
      numer[i] <- (nmat[i, 3]/sum(nmat[, 3])) #Proportion of tract 'variable' to total tract 
    }
    nmat <- cbind(nmat, denom) #Add proportions to numerator / denominator matrix 'nmat'
    nmat <- cbind(nmat, numer)
    
    #Calculate proportion of variable in cluster vs. cluster proportion of variable in population
    #A high 'Index' cluster has a relatively high concentration of 'variable' relative to the total population
    #AND the cluster's population is a relatively low proportion of the total population
    Index <- c() 
    for (i in 1:num_clust) {
      Index[i] <- (numer[i]/denom[i]) * 100
    }
    
    #Add 'Index' to 'nmat' and convert to data frame
    nmat <- cbind(nmat, Index)
    nmat <- data.frame(nmat)
    
    nmat <- nmat[order(nmat$Index, decreasing = TRUE), ] #Order by 'Index' 
    
    #Empty vectors for storing denominator & numerator 
    den <- c()
    num <- c()
    
    for (i in 1:num_clust) {
      den[i] <- nmat[i, 4] #Get denominator value for row
      num[i] <- nmat[i, 5] #Get numerator value for row 
    }
    
    #Store numerator & denominator of highest 'Index' entry 
    XK <- c()
    YK <- c()
    XK[1] <- den[1]
    YK[1] <- num[1]
    
    ##Start at the highest-share cluster and get cumulative cluster share in descending order## 
    #Iterate through all the other cluster categories 
    #Get the cumulative proportions of total population and 'variable'
    for (i in 2:num_clust) {
      XK[i] <- (XK[i - 1] + den[i]) #Cumulative total pop
      YK[i] <- (YK[i - 1] + num[i]) #Cumulative 'variable'
    }
    
    #Absolute value of cumulative 'variable' - cumulative pop by cluster type 
    absXminusY <- c()
    for (i in 1:num_clust) {
      absXminusY[i] <- abs(XK[i] - YK[i])
    }
    
    #Add cumulative pop & 'variable' shares to nmat 
    nmat <- cbind(nmat, XK)
    nmat <- cbind(nmat, YK)
    nmat <- cbind(nmat, absXminusY)
    
    
    "
    These are the pieces of Brown's (1994) Gini index equation (pp 1246, eq. 3)
    "
    #Vectors for storing pieces of equation
    
    A <- c() #Cluster cumulative share of 'variable' + previous cluster cumulative share of 'variable'
    B <- c() #Cluster cumulative share of pop - previous cluster cumulative share of pop 
    AxB <- c()
    
    for (i in 1:num_clust) {
      if (i == 1) { #For the first cluster category, just assign A & B that cluster's shares (no '0' entry)
        A[i] <- YK[i]
        B[i] <- XK[i]
      }
      if (i > 1) { #All other cluster categories 
        
        A[i] <- YK[i] + YK[i - 1] #Sum current cluster cumulative share of 'variable', previous cluster cumulative share of 'variable' 
        B[i] <- XK[i] - XK[i - 1] #Subtract current cluster cumulative share of pop, previous cluster cumulative share of pop
        
      }
    }
    
    #Get the products of equation pieces A & B of the Gini Index equation
    #A - 'variable' cumulative shares cluster-wise, in descending order 
    #B - pop cumulative shares cluster-wise, in descending order of 'variable' 
    for (i in 1:num_clust) {
      
      AxB[i] <- A[i] * B[i]
      
    }
    
    #Add pieces of the equation to nmat
    nmat <- cbind(nmat, A)
    nmat <- cbind(nmat, B)
    nmat <- cbind(nmat, AxB)
    
    ##Get Gini index for 'variable'##
    #Sum the AxB column of nmat and subtract from 1
    #Take absolute value - #**I don't see this in the Brown piece??** 
    Gini <- abs(1 - (sum(nmat[, 12])))
    
    ##Assign Gini coefficient to every cluster in the list 
    #**Why do we do this??**
    GiniCo <- c()
    for (i in 1:num_clust) {
      GiniCo[i] <- abs(1-sum(nmat[,12]))
    }
    nmat <- cbind(nmat, GiniCo)
    
    #Append nmat to 'master_list' 
    master_list[[k]] <- nmat 
    
  }
  
  #Data frame for storing cumulative shares by variable 
  #This will look like a melted df when we iterate through all variables 
  cheese <- data.frame()
  
  #Iterate through Gini coeff's by variable and append to 'cheese'
  
  for (i in 1:length(master_list)) {
    
    a <- master_list[[i]] #Get Gini coeff dataframe for the current variable 
    
    x <- c(0, a[, 7]) #Cumulative shares of current variable by cluster - append 0 for none 
    y <- c(0, a[, 8]) #Cumulative shares of population by cluster, ordered by current variable - append 0 for none 
    
    naa <- variables[i] #Get current variable name 
    nap <- nrow(a) + 1 #Get number of clusters + 1 for 0 shares 
    
    names <- rep(naa, nap) #Repeat the variable name by the number of clusters 
    
    #Combine cummulative shares & variable name 
    new <- cbind(x, y)
    new <- cbind(new, names)
    
    cheese <- rbind(cheese, new) #Append to cumulative shares by variable DF 
  }
  
  ##PLOT LORENZ CURVE PREP## 
  #We need to plot a line segment with slope=1 to match every cluster represented
  #in the Lorenz curve 
  half <- floor(num_clust/2)
  remainder <- num_clust - half
  dummy1 <- as.numeric(c(rep(0, half), rep(1, remainder + 1)))
  dummy2 <- as.numeric(c(rep(0, half), rep(1, remainder + 1)))
  brie <- cbind(dummy1, dummy2) #Df of clusters 
  
  dummy3 <- rep("x=y", nrow(a) + 1) #Group assignment for LC equality line
  brie <- cbind(brie, dummy3)
  
  colnames(brie) <- c("x", "y", "names") #Reassign 'brie' names for consistency with Gini coeff df 'cheese'
  
  cheese <- rbind(cheese, brie) #Append equality line 'brie' to the end of Gini coeff df 'cheese'
  
  #Convert 'cheese' proportions to numeric 
  #Necessary because 'brie' columns append to 'cheese' as characters
  cheese$y <- as.numeric(as.character(cheese$y))
  cheese$x <- as.numeric(as.character(cheese$x))
  
  ##PLOT LORENZ CURVES## 
  #   #The ggplot overlays the curves for each variable vs. perfect equality "x=y" 
  #   colours <- c(rep("steelblue", length(master_list)), "black")
  #   library(ggplot2)
  #   g <- ggplot(data = cheese, aes(x = y, y = x, group = names, 
  #                                  colour = names)) + ggtitle("Lorenz curves for specified variables") + 
  #     geom_line()
  #   print(g)
  #   
  ##Store the Gini coeff's in a vector 
  gini_vec <- c()
  # dev.new()
  for (i in 1:length(master_list)) {
    a <- master_list[[i]]
    b <- a[i, 13]
    gini_vec[i] <- b
  }
  
  return(gini_vec)
  
  #   #Sort Gini coefficients - **do we still need these layout commands??**
  #   layout(1)
  #   par(las = 2)
  #   par(mar = c(5, 8, 4, 2))
  #   gini_vec <- sort(gini_vec, decreasing = TRUE)
  
  #   for (i in 1:length(variables)) {
  #     cat("Variable: ", variables[i], "   ", "Gini Coefficient: ", 
  #         gini_vec[i], "\n")
  #   }
  #   news <- data.frame(cbind(gini_vec, variables))
  #   news$gini_vec <- as.numeric(as.character(news$gini_vec))
  #   gg <- ggplot(data = news, aes(x = variables, y = gini_vec, 
  #                                 group = variables)) + geom_bar() + ggtitle("Gini Coefficients for variables specified")
  #   print(gg)
  # 
  #   aa <- summary(gini_vec)
  #   sd <- sd(gini_vec)
  #   cat("Summary statistics for Gini Coefficients: ", "\n")
  #   print(aa)
  #   cat("Standard deviation: ", "\n")
  #   print(sd)
  #   cat("Returning List of Calculation Information","\n")
  
  #   ##Print Gini Coefficients by variable##
  #   gini_coefficients<-c()
  #   for(i in 1:length(variables)){
  #     cat("Variable:   ",variables[i],"    Gini Co.:    ",master_list[[i]][1,13],"\n")
  #     gini_coefficients[i]<-master_list[[i]][1,13]
  #   }
  #   cat("Mean Gini Co.:   ",mean(gini_coefficients),"\n")
  #   #master_list
  #   gini_coefficients
}

gini_getter<-function(inData){
  
  ggini<-data.frame(GEOID=as.character(inData$GEOID),type=paste0("C",1:nrow(inData)),inData[,2:ncol(inData)],stringsAsFactors = F)
  evaluate_class(data=ggini[,3:ncol(ggini)],cluster_lookup = ggini[,1:2],groups_or_types = "types",
                 variables=names(ggini)[4:ncol(ggini)],denominator_name = "TOTAL")
  
  
}

giniProfile<-function(inData,cluster=NULL,var.raw,mean.thresh=0.25,gini.thresh=0.25,verbose=T,get.output=F,out.csv=F){
  
  inData<-inData[inData[,2]==cluster,]
  
  cl.diag<-data.frame()
  for (v in 1:length(var.raw)){
    
    vvar<-var.raw[v][[1]]
    vvar<-vvar[vvar[,1] %in% inData$GEOID,]
    
    #safeguard - exclude empty variables
    excl.var<-c()
    for(i in 3:ncol(vvar)){ if(max(vvar[,i])==0){ excl.var<-c(excl.var,i) } }
    if(length(excl.var)>0){ vvar<-vvar[,-excl.var] }
    
    #Gini
    # print(names(var.raw)[v])
    giniv<-gini_getter(vvar)
    # print(giniv)
    # print(names(vvar)[-c(1:2)])
    names(giniv)<-names(vvar)[-c(1:2)]
    
    #Avg profile
    if(ncol(vvar)>3){
      
      ov<-toprop(var.raw[v][[1]])[,-1]
      ov<-ov[,names(ov) %in% names(vvar)]
      # print(head(ov))
      ov.mn<-colMeans(ov,na.rm=T)
      meanv<-(colMeans(toprop(vvar)[,-1])-ov.mn)/ov.mn
      mean.raw<-colMeans(toprop(vvar)[,-1])
      
    }else{
      
      ov.mn<-mean(toprop(var.raw[v][[1]])[,-1],na.rm=T)
      meanv<-(mean(toprop(vvar)[,-1])-ov.mn)/ov.mn
      mean.raw<-mean(toprop(vvar)[,-1])
      
    }
    
    cl.diag<-rbind(cl.diag,data.frame(var.gini=giniv,mean.raw=mean.raw,var.mean=meanv))
    
    
  }
  
  if(verbose){ print(paste("CLASS",cluster)) }
  
  ##High/even variables
  #sort by avg profile
  cl.diag.hi<-cl.diag[order(cl.diag$var.mean,decreasing=T),]
  cl.diag.hi<-cl.diag.hi[cl.diag.hi$var.mean>=mean.thresh,]
  
  #sort by gini
  cl.diag.desc<-cl.diag.hi[cl.diag.hi$var.gini<=gini.thresh,]
  
  if (verbose){
    
    print(round(cl.diag.desc[order(cl.diag.desc$var.mean,decreasing=T),],2))
    
  }
  
  if(out.csv){
    
    write.csv(round(cl.diag.desc[order(cl.diag.desc$var.mean,decreasing=T),],2),
              paste0("gini_out/cl",cluster,"_gini_out_high.csv"))
    
  }
  
  gini.out<-data.frame()
  if (get.output){
    
    gini.out<-rbind(gini.out,round(cl.diag.desc[order(cl.diag.desc$var.mean,decreasing=T),],2))
    
  }
  
  ##Low/even variables
  #sort by avg profile
  cl.diag.low<-cl.diag[order(cl.diag$var.mean),]
  cl.diag.low<-cl.diag.low[cl.diag.low$var.mean<=-mean.thresh,]
  
  #sort by gini
  cl.diag.desc<-cl.diag.low[cl.diag.low$var.gini<=gini.thresh,]
  
  if(verbose){
    
    print(round(cl.diag.desc[order(cl.diag.desc$var.mean),],2))
    
    cat(sep='\n')
    
  }
  
  if(out.csv){
    
    write.csv(round(cl.diag.desc[order(cl.diag.desc$var.mean),],2),
              paste0("gini_out/cl",cluster,"_gini_out_low.csv"))
    
  }
  
  if (get.output){
    
    gini.out<-rbind(gini.out,round(cl.diag.desc[order(cl.diag.desc$var.mean),],2))
    return(gini.out)
  }
  
}

giniBar<-function(inData,upper=NULL,lower=NULL,compare=F,plot.title=NULL){
  
  # rownames(inData)<-strwrap(rownames(inData),width = 100)
  # for(i in 1:length(rownames(inData))){ rownames(inData)[i]<-paste(strwrap(rownames(inData)[i],width=10),collapse="\n") }
  #   inData$variable<-rownames(inData)
  #   for(i in 1:length(inData$variable)){ inData$variable[i]<-paste(strwrap(inData$variable[i],width=10),collapse="\n") }
  inData$variable<-factor(rownames(inData),levels=rownames(inData[order(inData$var.mean,decreasing=T),]))
  
  plot1 <- qplot(x=variable,y=var.mean,data=inData,geom="bar",stat="identity",fill=var.gini)+
    scale_fill_continuous(low = lower,high=upper,limits=c(0,0.3))+
    theme(axis.text.x = element_text(size=16, angle=45, hjust=1, vjust=1))+
    ylab("Variable Average Profile") + 
    xlab("Variable") 
  
  if (compare){
    plot1<-plot1+facet_wrap(~series)
  }
  
  if (!is.null(plot.title)){
    
    plot1<-plot1+ggtitle(plot.title)
    
  }
  
  return(plot1)
  
}
