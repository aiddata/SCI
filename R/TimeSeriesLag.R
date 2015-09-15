TimeSeriesLag <- function(dta,yearID,unit_ID,lagNum,inName,outName,start_yr,end_yr)
{
  for(i in 1:length(dta[[1]]))
  {
  cur_id = dta[unit_ID][i,]
  cur_year = dta[yearID][i,]
  dta[outName][i,] <- dta[dta[yearID] == (cur_year-lagNum) & dta[unit_ID] == cur_id,][inName][1,]
  }
  return(dta)
}