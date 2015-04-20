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
  ttl = paste(title_pre," (", compvar,")", sep="")
  print(names(trtLen))
  plot(trtLen@ControlA)
  summary(trtLen@ControlA)
  bldstr = paste("ggplot(data=trtLen, aes(x=",compvar,",fill=trt)) + geom_density(alpha=0.2, aes(y=..count..))  + ggtitle(ttl)", sep="")
  #geom_bar defaults to stacking.

  print("bldstr and bld")
  print(bldstr)

  bld <- eval(parse(text=bldstr))
  print(bld)
  print("over----")
  return(bld)
}

ViewShp = function(dta,field,loc,z)
{
dta@data$id <- rownames(dta@data)
dta.points <- fortify(dta, region="id")
dta.df <- merge(dta.points, dta@data, by="id")
exec_str = paste("dta.df$Legend <- cut(dta.df$",field,", breaks=c(quantile(dta.df$",field,",probs=seq(0,1,by=0.2))))",sep="")
eval(parse(text=exec_str))
baseMap <- get_map(location=loc, zoom=z)
map <- ggmap(baseMap, extent='device') + geom_polygon(aes(fill=Legend,x=long,y=lat,group=group),
                                     data=dta.df,
                                     alpha=0.8,
                                     color="white",
                                     size=0.2                                
                                     ) + ggtitle(field)
                                    
#map <- ggplot(dta.df) + aes(long,lat,group=group,fill=Legend) + geom_polygon() + scale_fill_brewer(palette="PuRd") + ggtitle(field)
return(map)
}