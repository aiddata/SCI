#Tool 1: Descriptive Comparison of Treatment and Control

#shp - the shapefile to analyze
#flds - A 2-element list.  First item in first element is the treatment field, the rest are ancillary fields you want to
#describe against.  Second list defines data types.

#Currently supported methods:
#"thresh" - convert a continious treatment to a binary; requires 3 options in a 4-element list:
#The first element is "thresh"
#Right now, the second element of that can only be "manual", for manual threshold definition.
#The third element is then the threshold, and the fourth is the directionality ("<" or ">").
#Note the equality test is always "greater or equal" or "less or equal".

#Note, for ancilliary data, this currently accepts three types of data:
#time series - denoted by Name_timestamp, where timestamp is any ordered numeric value.
#factor - character-based data
#continious data


descTrtCont <- function(shp, flds, method){
  #Load and Create a temporary copy of the shapefile to work with.
  shp_file_tmp <- readShapePoly(shp)
  
  if(method[1] == "thresh")
  {
    if(method[2] == "manual")
    {      
      eval_statement = paste("shp_file_tmp$Trt <- ifelse(shp_file_tmp$",flds[[1]][1],method[4],method[3],",",1,",",0,")",sep="")
      eval(parse(text=eval_statement))
    }
  }
  #At this point in the function, it is expect shp_file_tmp$Trt has been defined
  #We will also do one check to see if it's a continious or binary value.
  if(BinCheck(shp_file_tmp$Trt) == "bin")
  {
    #At this point, we initialize our RShiny routines for Binary Treatment data.
    #We loop through each defined ancillary data product and produce the relevant descriptives:
    for (i in 2:length(flds[[1]]))
    {
      if(flds[[2]][i] == "time")
      {
       return("temporal dataset") 
      }
      if(flds[[2]][i] == "factor")
      {
        return("factor dataset") 
      }
      if(flds[[2]][i] == "con")
      {
        return("con dataset") 
      }
    }
  }
  if(BinCheck(shp_file_tmp$Trt) == "con")
  {
    return("Continious treatment representations are not yet supported, sorry!")
  }
  
  return(eval_statement)
}

descTrtCont(shp=user_shape, flds=list(c("Accepted_Y","NDVI_","UF","pop_FUNAI2"),c("con","time","factor","con")), method=c("thresh","manual",2002,"<"))
