GroupCompHist = function (dta, compvar, title_pre,simple_out)
{
  #Note, this function is currently sloppy and assumes the treatment is
  #Binary, and defined in "TrtBin"
  retData = dta
  treated <- retData@data[retData@data$TrtBin == 1,]
  untreated <- retData@data[retData@data$TrtBin == 0,]
  treated$trt = 'treated'
  untreated$trt = 'untreated'

  trtLen <- rbind(treated,untreated)
  
  if(grepl("factor",compvar))
  {
    print("This script currently does not support the visualization of factor variables.")
    return(NA)
  } else
  {  
  exec_str <- paste("trtLen$vizTEMP <- as.numeric(trtLen$",compvar,")",sep="")  
  eval(parse(text=exec_str))
  ttl = paste(title_pre," (", compvar,")", sep="")
  bldstr = paste("ggplot(data=trtLen, aes(x=vizTEMP,fill=trt)) + geom_density(alpha=0.2, aes(y=..density..))  + ggtitle(ttl) +xlab('",compvar,"')", sep="")
  #geom_bar defaults to stacking.

  bld <- eval(parse(text=bldstr))

  if(simple_out == TRUE)
  {
    print(bld)
  }
  return(bld)
  }
}

#PSM distance decay examination - should we enforce a threshold for matches or not?
#This is a wrapper for a heavily modified  sp.correlogram from SPDEP
#This new function (PSM_correlogram) allows for the specification of distance bands
#Neighbors within each band are then tested for Moran's I correlation.
PSMdistDecay = function(dta,psm_col,start,end,h)
{
  #Produce a corellogram using Moran's I at varying resolutions
  #First, convert to an equal-distance projection
  
  dta_prj_coords <- project(as.matrix(coordinates(dta)),"+proj=laea") 
  dta_prj <- as(dta,"data.frame")
  coordinates(dta_prj) <- dta_prj_coords
  
  exec <- paste("PSM_correlogram(as.matrix(coordinates(dta_prj)),dta_prj$",psm_col,",order=",h,",zero.policy=TRUE,start=",start,",end=",end,")",sep="")
  sp.cor <- eval(parse(text=exec))
  plot(sp.cor)
  return(sp.cor)
}

