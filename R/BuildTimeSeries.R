BuildTimeSeries <- function(dta,idField,varList_pre,startYear,endYear,colYears=NULL,interpYears=NULL)
{
  years <- startYear:endYear
  #If there is a "colYears" variable, convert to binaries.
  #Eventually could be extended to more than one column.
  if(!is.null(colYears))
  {
    #For each variable, for each year, create a binary representing the treatment status.
    for(k in 1:length(years))
    {
      varN <- paste("TrtMnt",years[k],sep="")
      exec <- paste("dta$",varN,"=0",sep="")
      eval(parse(text=exec))
      dta@data[varN][dta@data[colYears] <= as.numeric(years[k])] <- 1
    }
  }
  
  #If there is an "interpVars" variable, linearly interpolate values based on at least 2 known points in time.
  if(!is.null(interpYears))
  {
    interpFrame <- dta@data[idField]
    cnt = 1
    for(k in 1:length(years))
    {
    #First, build a model describing the relationship between years and any data in the interp field.
    varI <- paste(interpYears,years[[k]],sep="")
    #Check if data exists for the year - if not, ignore.  If so, include in the new modeling frame.
    
      if(varI %in% colnames(dta@data))
      {
        add_data <- paste("interpFrame[cnt] <- dta@data$",varI)
        eval(parse(text=add_data))
        cnt = cnt + 1
        colnames(interpFrame)[cnt] <- years[[k]]
      } else
      {
        #Exception for a single-point interpolation
        varC <- paste(interpYears,sep="")
        print(varC)
        if(varC %in% colnames(dta@data))
        {
          add_data <- paste("interpFrame[cnt] <- dta@data$",varC)
          eval(parse(text=add_data))
          cnt = 2
        }
      }
    
    }
    print(cnt)
    #Only one time point, so no interpolation is done - value is simply copied to all other columns.
    if(cnt == 2)
    {
      for(k in 1:length(years))
      {
        varI <- paste("dta@data$",interpYears,years[[k]]," <- interpFrame[2]",sep="")
      }
    } else {
    #Here, we model out everything. 
    #Melt the dataframe for modeling
    melt_Model_dta <- melt(interpFrame,id=idField)
    
    }
    View(melt_Model_dta)
    
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