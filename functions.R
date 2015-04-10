BinCheck = function(b, naVal="NA")
{
  bSet = unique(b)
  bSet = bSet[!is.na(bSet)]
  
  if(any(as.integer(bSet) != bSet)) "con"
  else if (length(bSet) > 2) "con"
  else "bin"
}

GroupCompHist = function (dta, compvar, title_pre)
{
  #Note, this function is currently sloppy and assumes the treatment is
  #Binary, and defined in "TrtBin"
  retData = dta
  treated <- retData@data[retData@data$TrtBin == 1,]
  untreated <- retData@data[retData@data$TrtBin == 0,]
  treated$trt = 'treated'
  untreated$trt = 'untreated'
  trtLen <- rbind(treated,untreated)
  ttl = paste(title_pre, compvar, sep=" ")
  bldstr = paste("print(ggplot(trtLen, aes(",compvar,",fill=trt)) + geom_density(alpha=0.2, aes(y=..count..))  + ggtitle(ttl))", sep="")
  #geom_bar defaults to stacking.
  eval(parse(text=bldstr))
}