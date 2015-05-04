#PSM distance decay examination - should we enforce a threshold for matches or not?
#This is a wrapper for a heavily modified  sp.correlogram from SPDEP
#This new function (PSM_correlogram) allows for the specification of distance bands
#Neighbors within each band are then tested for Moran's I correlation.
PSMdistDecay = function(dta,psm_col,start,end,h)
{
  #Produce a corellogram using Moran's I at varying resolutions
  #First, convert to an equal-distance projection
  
  #dta_prj_coords <- project(as.matrix(coordinates(dta)),"+proj=laea") 
  #dta_prj <- as(dta,"data.frame")
  coordinates(dta_prj) <- dta_prj_coords
  
  exec <- paste("PSM_correlogram(as.matrix(coordinates(dta_prj)),dta_prj$",psm_col,",order=",h,",zero.policy=TRUE,start=",start,",end=",end,")",sep="")
  sp.cor <- eval(parse(text=exec))
  plot(sp.cor)
  return(sp.cor)
}

