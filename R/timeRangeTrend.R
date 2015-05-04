timeRangeTrend <- function(dta,prefix,startyr,endyr,IDfield)
{
  grep_str = paste(IDfield,prefix,sep="|")
  tDF <- dta@data[grepl(grep_str,names(dta@data))]
  analysisDF <- melt(tDF,id=c(IDfield))
  
  #cleaned GREP
  new_pre <- gsub("[0-9]","",prefix,fixed=TRUE)
  analysisDF["Year"] <- lapply(analysisDF["variable"],FUN=function(x) as.numeric(gsub(new_pre,"",x)))
  analysisDF <- analysisDF[analysisDF["Year"] >= startyr ,]
  analysisDF <- analysisDF[analysisDF["Year"] <= endyr ,]
  dta@data["newfieldID"] <- 0
  for (i in 1:length(dta))
  {
    ID <- as.character(dta@data[IDfield][i,])
    #Fit trend model
    ID_dat <- analysisDF[analysisDF[IDfield] == ID,]
    trend_mod <- lm(value ~ Year,data=ID_dat)
    dta@data["newfieldID"][i,] <- summary(trend_mod)$coefficients[2]
  }
  return(dta$newfieldID)
  
}