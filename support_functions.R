"
Author: Joe Tuccillo

Description: Helper functions for data preparation, cluster analysis, 
  and diagnostics and visualization of results. 

Functions: 

  toProp: Converts count variables to a proportion of the source
    dataset total.

  plotMap: Plots a thematic map of one or more cluster solutions. 

  compareMv: Computes the mean vector of variables of interest by
    cluster groups. If specified, compares the mean vector of target
    variables to the overall mean vector. 

  plotHeat: Plots average profile characteristics of variables of interest
    by cluster groups in a 'heatmap' style raster plot. If specified, 
    compares multiple cluster solutions or domains of variables of interest.


"

library(rgeos)
library(maptools)
library(RColorBrewer)
library(cluster)
library(ggplot2)
library(reshape2)
library(fpc)

toprop<-function(inData){
  
  "
  Converts count variables to a proportion of the source
  dataset total.

  Arguments: 

    inData: data frame containing a geographic identifier ('GEOID')
      and total population ('TOTAL') field.

  Returns: data frame of input variables scaled as proportions of the 
    source dataset total. 
    
  "
  
  if(ncol(inData)>3){
    
    return(data.frame(GEOID=inData$GEOID,sapply(X=inData[,-c(1:2)],FUN=function(X){X/inData[,2]}))) 
    
  }else{ #safeguard if only one value field in addition to 'GEOID' and 'TOTAL'
    
    prop.out<-data.frame(GEOID=inData$GEOID)
    prop.out[[names(inData)[3]]]<-inData[,3]/inData[,2]
    return(prop.out)
    
  }
  
}

plotMap<-function(inGeom,inClust,geomKey="GEOID",clustKey="GEOID",compare=FALSE,map.title=NULL,
                  custom.colors=NULL,legend.key.extend=FALSE,plot.borders=T,plot.basemap=F,
                  base.geom=NULL,hide.gridlines=F,return.map=F){
  
  "
  Plots a thematic map of one or more cluster solutions. 
  
  Arguments:
  
    inGeom: a SpatialPolygons data frame consisting of cluster analysis aggregation units.  

    geomKey: 'inGeom' geographic identifier for merge (default is 'GEOID'). 

    inClust: a data frame consisting of a geographic identifier and cluster membership 
      ('cluster') fields.

    clustKey: 'inClust' geographic identifier for merge (default is 'GEOID'). 

    custom.colors: optional. A character vector of hex color codes for map categories. 
      Uses RColorBrewer 'Spectral' if none specified.

    map.title: an optional title for map.

    compare: optional. If TRUE, plots multiple maps using a 'series' field.

    legend.key.extend: optional. If TRUE, increase the height of the legend key items. This is
      useful when using extended legend labels.

    plot.borders: optional. If TRUE (default), add aggregation unit boundaries to map.title
  
    plot.basemap: optional. If TRUE, add outlines or boundaries to map using another 
      SpatialPolygons object.

    base.geom: optional. Basemap geometry, a SpatialPolygons object.
    
    hide.gridlines: optional. If TRUE, remove default ggplot background.

    return.map: optional. if TRUE, returns the ggplot map object.
  
  Returns: 
  
    The ggplot map object, when 'return.map=TRUE'. Default behavior is to return a
    plot in the R graphics device.
  
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


compareMv<-function(inData,verbose=F,return.means=F,plot.ready=T){
  
  "
  
  Computes the mean vector of variables of interest by cluster groups. 
  If specified, compares the mean vector of target variables to the overall 
  mean vector. 

  Arguments: 

    inData: data frame of cluster membership. First two columns must represent a 
      geographic identifier and cluster membership. 

    
    verbose: optional. If TRUE, print the overall mean vector of variables of interest
      to the console. 

  Returns: 

    If 'return.means=TRUE', returns the overall mean vectors of variable interest only.

    If 'return.means=FALSE' and 'plot.ready=FALSE', returns a data frame whose rows describe each 
      category mean and its average profile (percentage difference from the overall mean).

    Otherwise, default return ('plot.ready=T') is a matrix containing the percentage difference in 
    the mean vector of the variables of interest by group versus the overall mean. 

  "
  
  all.mean<-colMeans(inData[,-c(1:2)])
  inData<-aggregate(x=inData[,3:length(inData)],by=list(inData[,2]),na.rm=T,FUN=mean)
  
  if (verbose){
    cat(sep='\n\n')
    cat(paste("Variable Averages:",toString(names(all.mean))),all.mean,sep="\n")
  }
  
  if(return.means){ #Return the overall mean only 
    
    return(all.mean) 
    
  }else{
    
    inData.avp<-inData #generate "plot.ready" version of our input DF (average profiles)
    for (i in 2:ncol(inData.avp)){inData.avp[,i]<-round(100*((inData.avp[,i]-all.mean[i-1])/all.mean[i-1]))}
    
    if(plot.ready){ #return the 'plot.ready' df only 
      
      return(inData.avp)
      
    }else{ #return a combination of the average profiles and means 
      
      #Append category means to average profiles 
      out.avp<-data.frame(Type="ALL",t(round(all.mean,2)),stringsAsFactors = F)
      names(out.avp)[-1]<-names(all.mean)
      rownames(out.avp)<-"Overall Mean"
      for(j in 1:nrow(inData.avp)){ 
        
        catnm<-inData[j,][,1] #Get cluster type name (safeguard if using non-numeric type names)
      
        avp_mean<-rbind(inData[j,],inData.avp[j,])
        names(avp_mean)[1]<-"Type"
        
        rownames(avp_mean)<-c(paste("Type",catnm,"Mean"),
                              paste("Average Profile, Type",catnm))
        
        out.avp<-rbind(out.avp,avp_mean)
        
      }
      
      return(out.avp)
      
    }
    
  }

  
}

plotHeat <- function(inData,transpose=FALSE,flip=FALSE,compare=FALSE,series.list=NULL,return.heatMap=F){
  
  "
  Plots average profile characteristics of variables of interest by cluster groups
  in a 'heatmap' style raster plot. If specified, compares multiple cluster solutions or
  domains of variables of interest.
  
  Arguments: 
  
    inData: data frame of cluster membership. First two columns must represent a 
      geographic identifier and cluster membership. 
    
    transpose: optional. If TRUE, transpose the heatmap (X: cluster categories, Y: variables).
    
    flip: optional. If TRUE, reverse cardinality of values.
    
    compare: optional. If TRUE, plots multiple heatmaps using 'series.list'.
    
    series.list: optional. Used to define heatmap groups when 'compare=TRUE'.
    
    return.heatMap: optional. If TRUE, returns the heatmap ggplot object.
  
  Returns: 
  
    The ggplot map object, when 'return.heatMap=TRUE'. Default behavior is to return a
    plot in the R graphics device.
  
  
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
    
    
#     plot(heatMap)
    
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
    
#     plot(heatMap)
    
  }
  
  
  if(return.heatMap){
    
    return(heatMap)
    
  }else{
    
    plot(heatMap)
  }

}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
