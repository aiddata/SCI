SpatialCausalPSM <- function(dta, mtd,mdl,drop)
{
  #First, we check for methods to fit the PSM.
  if(mtd == "logit")
  {
    PSMfit <- glm(mdl, dta@data, family="binomial")
    retData <- dta
    retData$PSM_trtProb <- predict(PSMfit, dta@data, type="response")
  }
  
  #Show user distributions.
  treated <- retData@data[retData@data$TrtBin == 1,]
  untreated <- retData@data[retData@data$TrtBin == 0,]
  treated$trt = 'treated'
  untreated$trt = 'untreated'
  trtLen <- rbind(treated,untreated)
  print(ggplot(trtLen, aes(PSM_trtProb, fill=trt)) + geom_density(alpha=0.3) + ggtitle("Trt/Cont PSM Values"))
  
  #Second, if a drop parameter - if set to "overlap", remove observations
  #that don't overlap in the PSM distribution.
  if(drop == "overlap")
  {
    
    #Drop
    min_cut <- max(min(treated$PSM_trtProb), min(untreated$PSM_trtProb))
    max_cut <- min(max(treated$PSM_trtProb), max(untreated$PSM_trtProb))
    
    retData <- retData[retData@data$PSM_trtProb >= min_cut,]    
    retData <- retData[retData@data$PSM_trtProb <= max_cut,] 
    
    #Post drop histograms
    treated <- retData@data[retData@data$TrtBin == 1,]
    untreated <- retData@data[retData@data$TrtBin == 0,]
    treated$trt = 'treated'
    untreated$trt = 'untreated'
    trtLen <- rbind(treated,untreated)
    print(ggplot(trtLen, aes(PSM_trtProb, fill=trt)) + geom_density(alpha=0.3) + ggtitle("Post-Extrapolation Drops"))
  }
  
  return (retData)
}

SpatialCausalDist <- function(dta, mtd)
{
  #Plot the pre-dropping balance on controls used for PSM model...
  
  if (mtd == "fastNN")
  {
    #Fast nearest neighbors search - will not arrive at optimum,
    #but this may not be an issue for many analysis.
    #-------Need to add in the non-optimal NN loop here, rank by PSM (highest goes first).
    print("non-opt")
  }
  if (mtd == "optNN")
  {
    print("To contain a nearest-neighbor algorithm...")
  }
}