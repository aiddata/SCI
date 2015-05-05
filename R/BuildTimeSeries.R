BuildTimeSeries <- function(dta,idField,varList_pre)
{
  #Run the melts
  meltList <- list()
  for (i in 1:length(varList_pre))
  {
    grep_str = paste(idField,varList_pre[i],sep="|")
    tDF <- dta@data[grepl(grep_str,names(dta@data))]

    meltList[[i]] <- melt(tDF,id=idField)

  }

  #Merge the melts together
  retDF <- merge(meltList[[1]],meltList[[2]],meltList[[3]])
  return(retDF)
}


# dm1 <- melt(d[,c("Type","I.alt","idx06","idx07","idx08")], id=c("Type","I.alt"))
# dm2 <- melt(d[,c("Type","I.alt","farve1","farve2")], id=c("Type","I.alt"))
# colnames(dm2) <- c("Type", "I.alt", "variable2", "value2")
# dm <- merge(dm1, dm2)