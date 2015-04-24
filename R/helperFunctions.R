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