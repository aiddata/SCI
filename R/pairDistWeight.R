#Model the distance-decay for a given pair.
#Nice details on these models here:
#https://v8doc.sas.com/sashtml/stat/chap34/sect12.htm

pairDistWeight <- function(dist,threshold,type)
{
  if(type == "GausSemiVar")
  {
    return(1 - exp(-(dist^2/(threshold)^2)))
  }
  if(type == "ExpSemiVar")
  {
    return(1 - exp(-(dist/(threshold))))
  }
  if(type=="Spherical")
  {
    if(dist>=threshold)
    {
      return(1)
    } else {
      return( (3/2) * (dist/threshold) - (1 / 2) * (dist/threshold)^3 )
    }
  }
  
}