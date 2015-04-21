SpatialCausalPSM <- function(dta, mtd,mdl,drop, visual)
{
  #Initialization
  pltObjs <- list()
  
  #Method
  if(mtd == "logit")
  {
    PSMfit <- glm(mdl, dta@data, family="binomial")
    retData <- dta
    retData$PSM_trtProb <- predict(PSMfit, dta@data, type="response")
  }
  if(mtd=="lm")
  {
    PSMfit <- lm(mdl, dta@data)
    retData <- dta
    retData$PSM_trtProb <- predict(PSMfit, dta@data, type="response")
  }
  
  if(visual == "TRUE")
  {
    #Show user distributions.
    pltObjs[[1]] <- GroupCompHist(retData, "PSM_trtProb","Initial PSM Balance",simple_out=FALSE)
    print(summary(PSMfit))
  }

  
  #Second, if a drop parameter - if set to "overlap", remove observations
  #that don't overlap in the PSM distribution.
  if(drop == "overlap")
  {
    
    #Drop
    treated <- retData@data[retData@data$TrtBin == 1,]
    untreated <- retData@data[retData@data$TrtBin == 0,]
    min_cut <- max(min(treated$PSM_trtProb), min(untreated$PSM_trtProb))
    max_cut <- min(max(treated$PSM_trtProb), max(untreated$PSM_trtProb))
    
    retData <- retData[retData@data$PSM_trtProb >= min_cut,]    
    retData <- retData[retData@data$PSM_trtProb <= max_cut,] 
    
    if(visual == "TRUE")
    {
    #Post drop histograms
    pltObjs[[2]] <- GroupCompHist(retData, "PSM_trtProb","Post-Extrapolation Drops",simple_out=FALSE)
      
    #Output graphics
    grid.arrange(pltObjs[[1]], pltObjs[[2]],ncol=2,main="PSM Matching Stage 1 (Dropping Observations Requiring Extrapolation)")
    
    }
  }
  return (retData)
}

SpatialCausalSim_DGP<- function(fld_size, SpatialCov_opt,rho_mult)
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
  yfunc <- function(a, b, c) (0.0+(1.0*a)+(1.0*b)+abs(1.0*c))
  f.SPDF@data["y"] = apply(f.SPDF@data[,c('TrtBin','ControlA','RandomFieldB')], 1, function(y) yfunc(y['TrtBin'],y['ControlA'],y['RandomFieldB']))

  f.SPDF@data <- cbind(simIDs = rownames(f.SPDF@data), f.SPDF@data)
  
  return(f.SPDF)
}