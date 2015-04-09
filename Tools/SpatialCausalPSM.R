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
  print(ggplot(trtLen, aes(PSM_trtProb, fill=trt)) + geom_density(alpha=0.2) + ggtitle("Trt/Cont PSM Values"))
  
  #Second, if a drop parameter - if set to "overlap", remove observations
  #that don't overlap in the PSM distribution.
  if(drop == "overlap")
  {
    
    #Drop
    min_cut <- max(min(treated$PSM_trtProb), min(untreated$PSM_trtProb))
    max_cut <- min(max(treated$PSM_trtProb), max(untreated$PSM_trtProb))
    
    retData@data <- retData@data[retData@data$PSM_trtProb >= min_cut,]    
    retData@data <- retData@data[retData@data$PSM_trtProb <= max_cut,] 
    
    #Post drop histograms
    treated <- retData@data[retData@data$TrtBin == 1,]
    untreated <- retData@data[retData@data$TrtBin == 0,]
    treated$trt = 'treated'
    untreated$trt = 'untreated'
    trtLen <- rbind(treated,untreated)
    print(ggplot(trtLen, aes(PSM_trtProb, fill=trt)) + geom_density(alpha=0.2) + ggtitle("Post-Extrapolation Drops"))
  }
  
  return (retData)
}

SpatialCausalDist <- function(dta, mtd)
{
  if (mtd = "optNN")
  {
    print("To contain a nearest-neighbor algorithm...")
  }
}