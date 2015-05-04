BinCheck = function(b, naVal="NA")
{
  bSet = unique(b)
  bSet = bSet[!is.na(bSet)]
  
  if(any(as.integer(bSet) != bSet)) "con"
  else if (length(bSet) > 2) "con"
  else "bin"
}

timeRangeAvg <- function(dta,prefix,startyr,endyr)
{
  searchS = paste("^",prefix,startyr,sep="")
  searchE = paste("^",prefix,endyr,sep="")
  strt_id <- grep(searchS,colnames(dta))
  end_id <- grep(searchE,colnames(dta))
  rmean <- rowMeans(dta[strt_id:end_id])
  return(rmean)
}

timeRangeTrend <- function(dta,prefix,startyr,endyr,IDfield)
{
  grep_str = paste(IDfield,prefix,sep="|")
  tDF <- dta@data[grepl(grep_str,names(dta@data))]
  analysisDF <- melt(tDF,id=c(IDfield))

  #cleaned GREP
  new_pre <- gsub("[0-9]","",prefix,fixed=TRUE)
  analysisDF["Year"] <- lapply(analysisDF["variable"],FUN=function(x) as.numeric(gsub(new_pre,"",x)))
  analysisDF <- analysisDF[analysisDF["Year"] >= startyr ,]
  analysisDF <- analysisDF[analysisDF["Year"] <= endyr ,]
  dta@data["newfieldID"] <- 0
  for (i in 1:length(dta))
  {
    ID <- as.character(dta@data[IDfield][i,])
    #Fit trend model
    ID_dat <- analysisDF[analysisDF[IDfield] == ID,]
    trend_mod <- lm(value ~ Year,data=ID_dat)
    dta@data["newfieldID"][i,] <- summary(trend_mod)$coefficients[2]
  }
  return(dta$newfieldID)
  
}

#Slightly modified from Gmisc-package
print.htmlTable<- function(x, useViewer = TRUE, ...){
  # Don't use viewer if in knitr
  if (useViewer &&
        !"package:knitr" %in% search()){

    htmlFile <- tempfile(fileext=".html")
    htmlPage <- paste("<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
                      "</head>",
                      "<body>",
              
                      x,
                   
                      "</body>",
                      "</html>", sep="\n")
    cat(htmlPage, file=htmlFile)

    viewer <- getOption("viewer")

    if (!is.null(viewer) &&
          is.function(viewer)){
      # (code to write some content to the file)

      viewer(htmlFile)
    }else{
      utils::browseURL(htmlFile)
    }
  }else{

    cat(x)
  }
}


#Helper function to remove bad rows from a shapefile
removeRow <- function(dta,columnID,matchIDs)
{
  for (i in 1:length(dta))
  {
    exec_str <- paste("dta <- dta[dta@data$",columnID," !=",matchIDs[i],",]",sep="")
    print(exec_str)
    eval(parse(text=exec_str))
  }
  return(dta)
}

