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

  
  #Second, if a drop parameter - if set to "support", remove observations
  #that don't overlap in the PSM distribution.
  if(drop == "support")
  {
    
    #Drop
    treated <- retData@data[retData@data$TrtBin == 1,]
    untreated <- retData@data[retData@data$TrtBin == 0,]
    min_cut <- max(min(treated$PSM_trtProb), min(untreated$PSM_trtProb))
    max_cut <- min(max(treated$PSM_trtProb), max(untreated$PSM_trtProb))
    
    retData <- retData[retData@data$PSM_trtProb >= min_cut,]    
    retData <- retData[retData@data$PSM_trtProb <= max_cut,] 

  }
  if(visual == "TRUE")
  {
    #Post drop histograms
    pltObjs[[2]] <- GroupCompHist(retData, "PSM_trtProb","Post-Extrapolation Drops (if enabled)",simple_out=FALSE)
    
    #Output graphics
    grid.arrange(pltObjs[[1]], pltObjs[[2]],ncol=2,main="PSM Matching Stage 1 (Dropping Observations Requiring Extrapolation)")
    
  }
  return (retData)
}


