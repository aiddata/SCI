BinCheck = function(b, naVal="NA")
{
  bSet = unique(b)
  bSet = bSet[!is.na(bSet)]
  
  if(any(as.integer(bSet) != bSet)) "con"
  else if (length(bSet) > 2) "con"
  else "bin"
}

timeRangeAvg <- function(dta,prefix,startyr,endyr)
{
  searchS = paste("^",prefix,startyr,sep="")
  searchE = paste("^",prefix,endyr,sep="")
  strt_id <- grep(searchS,colnames(dta))
  end_id <- grep(searchE,colnames(dta))
  rmean <- rowMeans(dta[strt_id:end_id])
  return(rmean)
}

timeRangeTrend <- function(dta,prefix,startyr,endyr,IDfield,newfieldID)
{
  grep_str = paste(IDfield,prefix,sep="|")
  tDF <- dta@data[grepl(grep_str,names(dta@data))]
  analysisDF <- melt(tDF,id=c(IDfield))

  #cleaned GREP
  new_pre <- sub("[0-9]","",prefix)
  
  analysisDF["Year"] <- lapply(analysisDF["variable"],FUN=function(x) as.numeric(gsub(new_pre,"",x)))
  analysisDF <- analysisDF[analysisDF["Year"] >= startyr ,]
  analysisDF <- analysisDF[analysisDF["Year"] <= endyr ,]
  dta@data[newfieldID] <- 0
  for (i in 1:length(dta))
  {
    ID <- as.character(dta@data[IDfield][i,])
    #Fit trend model
    trend_mod <- lm(value ~ Year,data=analysisDF)
    dta@data[newfieldID][i,] <- summary(trend_mod)$coefficients[2]
  }
  return(dta)
  
}