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

ViewTimeSeries <- function(dta,IDfield,TrtField,idPre)
{
  grep_str = paste(idPre,IDfield,TrtField,sep="|")
  tDF <- dta@data[grepl(grep_str,names(dta@data))]
  vizDF <- melt(tDF,id=c(IDfield,TrtField))
  ggplot_exec <- paste(" 
    ggplot(data = vizDF, aes(x=variable, y=value, group=",IDfield,",colour=factor(",TrtField,"))) + 
    geom_line(size=.5, linetype=3) + 
    stat_summary(fun.y=mean,aes(x=variable, y=value, group=",TrtField,",colour=factor(",TrtField,")),data=vizDF,geom='line',size=1.5) +
    theme(axis.text.x=element_text(angle=90,hjust=1))",sep="")
  gg_ret <- eval(parse(text=ggplot_exec))
  print(gg_ret)
  return(gg_ret)
  
}