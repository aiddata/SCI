BuildTimeSeries <- function(dta,idField,varList_pre,startYear,endYear,TrtYears=NULL)
{
  years <- startYear:endYear
  #If there is a "TrtYears" variable, convert to binaries.
  #Eventually could be extended to more than one column.
  if(!is.null(TrtYears))
  {
    #For each variable, for each year, create a binary representing the treatment status.
    for(k in 1:length(years))
    {
      varN <- paste("TrtMnt",years[k],sep="")
      dta[varN] = 0
      View(dta[TrtYears])
      dta[varN][dta[TrtYears] >= years[k]] <- 1
    }
    View(dta["TrtMnt1996"])
  }
  
  #Run the melts

  meltList <- list()
  for (i in 1:length(varList_pre))
  {
    #grep_str = paste(idField,"|",varList_pre[i],"[0-9][0-9][0-9][0-9]",sep="")
    #Limit to only relevant years
    grepStrYrs = idField
    for(j in 1:length(years))
    {
      tempGrep <- grepStrYrs
      grepStrYrs <- paste(tempGrep,"|",varList_pre[[i]],years[[j]],sep="")
    }
    tDF <- dta@data[grepl(grepStrYrs,names(dta@data))]
    meltList[[i]] <- melt(tDF,id=idField)
    
    #Keep only years in the year column, rename columns
    colnames(meltList[[i]])[2] <- "Year"
    colnames(meltList[[i]])[3] <- varList_pre[[i]]
    
    #Clean up year column
    gsub_command <- paste("^",varList_pre[[i]],sep="")
    meltList[[i]][2] <- gsub(gsub_command, "", as.matrix(meltList[[i]][2]))
    
    
    #Remove ID and year if this is at least the second variable to avoid duplications.
    if(i > 1)
    {
      meltList[[i]] <- meltList[[i]][3]
    }

  }

  #Finish up with a cherry on top
  meltListRet <- data.frame(meltList)
  
  return(meltListRet)
}


# dm1 <- melt(d[,c("Type","I.alt","idx06","idx07","idx08")], id=c("Type","I.alt"))
# dm2 <- melt(d[,c("Type","I.alt","farve1","farve2")], id=c("Type","I.alt"))
# colnames(dm2) <- c("Type", "I.alt", "variable2", "value2")
# dm <- merge(dm1, dm2)