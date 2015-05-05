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
  return(gg_ret)
  
}