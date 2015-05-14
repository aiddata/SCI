GroupCompHist = function (dta, compvar, title_pre,simple_out)
{
  #Note, this function is currently sloppy and assumes the treatment is
  #Binary, and defined in "TrtBin"
  retData = dta
  treated <- retData@data[retData@data$TrtBin == 1,]
  untreated <- retData@data[retData@data$TrtBin == 0,]
  treated$trt = 'treated'
  untreated$trt = 'untreated'
  
  trtLen <- rbind(treated,untreated)
  
  if(grepl("factor",compvar))
  {
    print("This script currently does not support the visualization of factor variables.")
    return(NA)
  } else
  {  
    exec_str <- paste("trtLen$vizTEMP <- as.numeric(trtLen$",compvar,")",sep="")  
    eval(parse(text=exec_str))
    ttl = paste(title_pre," (", compvar,")", sep="")
    bldstr = paste("ggplot(data=trtLen, aes(x=vizTEMP,fill=trt)) + geom_density(alpha=0.2, aes(y=..density..))  + ggtitle(ttl) +xlab('",compvar,"')", sep="")
    #geom_bar defaults to stacking.
    
    bld <- eval(parse(text=bldstr))
    
    if(simple_out == TRUE)
    {
      print(bld)
    }
    return(bld)
  }
}