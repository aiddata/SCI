timeRangeAvg <- function(dta,prefix,startyr,endyr)
{
  searchS = paste("^",prefix,startyr,sep="")
  searchE = paste("^",prefix,endyr,sep="")
  strt_id <- grep(searchS,colnames(dta))
  end_id <- grep(searchE,colnames(dta))
  rmean <- rowMeans(dta[strt_id:end_id])
  return(rmean)
}