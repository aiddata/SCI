BuildTimeSeries <- function(dta,idField,varList_pre)
{
  grep_str = idField
  for (i in 1:length(varList_pre))
  {
    grep_str = paste(grep_str,varList_pre[i],sep="|")
  }
  print(grep_str)
  tDF <- dta@data[grepl(grep_str,names(dta@data))]
  retDF <- melt(tDF,id=c(idField))
  return(retDF)
}