SpatialCausalSim_DGP<- function(fld_size, SpatialCov_opt,rho_mult,B1,B2,B3)
{
  x <- fld_size
  RFoptions(seed=NA)
  
  model <- RMexp()
  z <- RFsimulate(model, x, x, n=6)
  f.SPDF <- as(z, 'SpatialPolygonsDataFrame')
  
  colnames(f.SPDF@data)[1] <- "ControlA"
  colnames(f.SPDF@data)[2] <- "RandomFieldA"
  colnames(f.SPDF@data)[3] <- "ControlB"
  colnames(f.SPDF@data)[4] <- "RandomFieldB"
  colnames(f.SPDF@data)[5] <- "TrtBin"
  colnames(f.SPDF@data)[6] <- "tmpLag"
  
  #Amplify spatial correlation to an arbitrary degree in each dataset.
  #This is to account for the issue that gaussian random fields are highly unlikely to have
  #Extreme values of spatial correlation.
  f.NB = poly2nb(f.SPDF)
  f.W = nb2listw(f.NB, style='W')
  
  for (i in 1:5)
  {
    if(grepl(colnames(f.SPDF@data)[i],SpatialCov_opt) == 1)
    {
      rho = runif(1,0.1,(1*rho_mult))
      ev_st = paste("eleLag <- lag.listw(f.W, f.SPDF@data[[",i,"]])",sep="")
      eval(parse(text=ev_st))
      f.SPDF@data["tmpLag"]=data.frame(eleLag)
      ev_stB = paste("f.SPDF@data[",i,"] = scale((f.SPDF@data['tmpLag']*rho) + f.SPDF@data[",i,"])",sep="")
      eval(parse(text=ev_stB)) 
    } else {
      ev_stC = paste("f.SPDF@data[",i,"] = rnorm(nrow(f.SPDF))",sep="")
      eval(parse(text=ev_stC))
    }
    
  }
  
  f.SPDF@data["TrtBin"] = f.SPDF@data["ControlA"] + f.SPDF@data["RandomFieldA"]
  #Convert the Treatment to a Binary
  #Create a temporary field
  f.SPDF@data$TrtTemp <- f.SPDF@data$TrtBin
  med_treat = median(f.SPDF@data$TrtTemp)
  f.SPDF@data$TrtBin[which(f.SPDF@data$TrtTemp >= med_treat )] <- 1 
  f.SPDF@data$TrtBin[which(f.SPDF@data$TrtTemp < med_treat )] <- 0 
  f.SPDF@data <- f.SPDF@data[,!names(f.SPDF) %in% c("TrtTemp")]
  
  #Build outcome variable for testing
  #y = intercept + (Theta * Treatment) + (Beta * ControlA)
  yfunc <- function(a, b, c) (0.0+(B1*a)+(B2*b)+abs(B3*c))
  f.SPDF@data["y"] = apply(f.SPDF@data[,c('TrtBin','ControlA','RandomFieldB')], 1, function(y) yfunc(y['TrtBin'],y['ControlA'],y['RandomFieldB']))
  
  f.SPDF@data <- cbind(simIDs = rownames(f.SPDF@data), f.SPDF@data)
  
  return(f.SPDF)
}