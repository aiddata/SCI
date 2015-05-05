BuildTimeSeries <- function(dta,idField,varList_pre,startYear,endYear)
{
  #Run the melts
  years <- startYear:endYear
  print(years)

  meltList <- list()
  for (i in 1:length(varList_pre))
  {
    grep_str = paste(idField,varList_pre[i],sep="|")
    tDF <- dta@data[grepl(grep_str,names(dta@data))]

    #Limit to only relevant years
    grepStrYrs = ""
    for(j in 1:length(years))
    {
      grepStrYrs <- paste(varList_pre[[i]],grepStrYrs,years[[j]],sep="|")
    }
    
    tDF <- tDF[grepl(grepStrYrs,tDF$variable)]
    
    meltList[[i]] <- melt(tDF,id=idField)
    #colnames(meltList[[i]][3]) <- "Test"
    
    #if(exists("retDF"))
    #{
    #  retDF <- merge(retDF,meltList[[i]],by=idField)
    #} else {
    #  retDF <- meltList[[i]]
    #}

  }

  return(meltList)
}


# dm1 <- melt(d[,c("Type","I.alt","idx06","idx07","idx08")], id=c("Type","I.alt"))
# dm2 <- melt(d[,c("Type","I.alt","farve1","farve2")], id=c("Type","I.alt"))
# colnames(dm2) <- c("Type", "I.alt", "variable2", "value2")
# dm <- merge(dm1, dm2)