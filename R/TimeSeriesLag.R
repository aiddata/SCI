TimeSeriesLag <- function(dta,yearID,unit_ID,lagNum,inName,outName,start_yr,end_yr)
{
  for(i in length(dta))
  {
    cur_id = dta[unit_ID][i,]
    cur_year = start_yr
    while(cur_year <= end_yr)
    {
      dta[outName][i,] <- dta[inName][which(data[yearID] == cur_year-lagNum && data[unit_ID] == cur_id)]
      cur_year = cur_year + 1
    }
  }
}