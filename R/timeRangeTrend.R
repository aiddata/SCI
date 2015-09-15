# run linear model on data within year range as specified
# by field prefix and return coefficients
timeRangeTrend <- function(dta, prefix, startyr, endyr, IDfield, thresh=0.5) {

    # # create new dataframe from all columns in dta dataframe that 
    # # are either the ID or a year which is indicated by the prefix
    # grep_str = paste(IDfield, prefix, sep="|")
    # tDF <- dta@data[grepl(grep_str, names(dta@data))]

    # # melt all years columns in new dataframe
    # analysisDF <- melt(tDF, id=c(IDfield))
    
    # # cleaned GREP - remove year digit placeholders
    # # new_pre <- gsub("[0-9]", "", prefix, fixed=TRUE)

    # # get location of year in prefix
    # yIndex <- regexpr("[0-9]", prefix, fixed=TRUE)

    # # generate new year field by removing prefix from variable (original column names)
    # # analysisDF["Year"] <- lapply(analysisDF["variable"], FUN=function(x) as.numeric(gsub(new_pre, "", x)))
    # analysisDF["Year"] <- lapply(analysisDF["variable"], FUN=function(x) as.numeric(substr(x, yIndex[1], yIndex[1]+3)))

    # # keep years in range specified
    # analysisDF <- analysisDF[analysisDF["Year"] >= startyr ,]
    # analysisDF <- analysisDF[analysisDF["Year"] <= endyr ,]

    # # create empty field
    # dta@data["newfieldID"] <- 0

    # # iterate over original dataframe
    # for (i in 1:length(dta)) {
    #     # get id for row (in original data)
    #     ID <- as.character(dta@data[IDfield][i,])

    #     # get all data corresponding to id from analysis dataframe
    #     ID_dat <- analysisDF[analysisDF[IDfield] == ID,]

    #     dat_length <-length(ID_dat)
    #     count_na <-sum(is.na(ID_dat[['value']])) 
    #     count_non_na <- dat_length - count_na
    #     percent_na <- count_na / dat_length

    #     # if number of NAs is over threshold or if less than 2 points of data are not NA, return NA
    #     if (percent_na > thresh || count_non_na < 2) {
            
    #         dta@data["newfieldID"][i,] <- NA
        
    #     } else {
    #         # fit trend model
    #         trend_mod <- lm(value ~ Year, data=ID_dat, na.action = na.omit)
      
    #         # add trend coefficients to new field
    #         dta@data["newfieldID"][i,] <- summary(trend_mod)$coefficients[2]
    #     }

    # }
    
    # # return new field with trend coefficients
    # return(dta[["newfieldID"]])
    
}
