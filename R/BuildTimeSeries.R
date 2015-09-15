BuildTimeSeries <- function (dta, idField, varList_pre, startYear, endYear, colYears=NULL, interpYears=NULL) {
    
    years <- startYear:endYear

    print("bts1")
    timer <- proc.time()

    # If there is a "colYears" variable, convert to binaries.
    # Eventually could be extended to more than one column.
    if (!is.null(colYears)) {
        # For each variable, for each year, create a binary representing the treatment status.
        for (k in 1:length(years)) {
            for (j in 1:length(colYears)) {

                varN <- paste("TrtMnt_",colYears[j],"_",years[k], sep="")
                print(varN)

                exec <- paste("dta$",varN," = 0", sep="")
                eval(parse(text=exec))
                # dta[,varN] = 0


                dta@data[varN][dta@data[colYears[j]] <= as.numeric(years[k])] <- 1
            }
        }
    }

    timer <- proc.time() - timer
    print(paste("section completed in", timer[3], "seconds."))

    print(colnames(dta@data))

    print("bts2")
    timer <- proc.time()


    # add the "TrtMnt_" + colYears[j] prefix to interpYears
    for (j in 1:length(colYears)) {
        trt_id = paste("TrtMnt_",colYears[j],"_####", sep="")
        interpYears <- c(interpYears, trt_id)  
    }

    print(interpYears)

    timer <- proc.time() - timer
    print(paste("section completed in", timer[3], "seconds."))


    print("bts3")
    timer <- proc.time()
    
    # If there is an "interpVars" variable, linearly interpolate values based on at least 2 known points in time.
    if (!is.null(interpYears)) {
        print("bts3.0")
        for (AncInt in 1:length(interpYears)) {
            print(interpYears[AncInt])

            print("bts3.0.0")
            cur_ancVi <- interpYears[AncInt]
            interpFrame <- dta@data[idField]
            interpFrame[idField] <- dta@data[idField]
            cnt = 2

            print("bts3.0.1")
            for (k in 1:length(years)) {
                # First, build a model describing the relationship between years and any data in the interp field.

                # Check if data exists for the year - if not, ignore.  If so, include in the new modeling frame.
                varI <- gsub('####', years[[k]], cur_ancVi)
                if (varI %in% colnames(dta@data)) {

                    interpFrame[cnt] <- dta@data[[varI]]
                    colnames(interpFrame)[cnt] <- years[[k]]
                    cnt = cnt + 1

                } else if (cur_ancVi %in% colnames(dta@data)) {

                    # Exception for a single-point interpolation
                    interpFrame[cnt] <- dta@data[[cur_ancVi]]
                    cnt = 3
                    
                }
            }

            print(cnt)

            print("bts3.0.2")
            # this is a slow part

            # Only one time point, so no interpolation is done - value is simply copied to all other columns.
            if (cnt == 3) {
                print("bts3.0.2a")
                for (k in 1:length(years)) {
                    # add _year to end of non temporal data
                    dta@data[[paste(cur_ancVi,years[[k]],sep="_")]] <- interpFrame[2]
                }

            } else if (cnt < length(years) + 2) {
                print("bts3.0.2b0")
                tDframe <- dta@data[idField]

                # Here, we model out everything. 

                # Melt the dataframe for modeling
                melt_Model_dta <- melt(data.frame(interpFrame), id=idField)
                melt_Model_dta["variable"] <- as.numeric(gsub("X", "", melt_Model_dta[["variable"]]))
                

                # Fit the model for interpolation
                print("bts3.0.2b1")
                
                execstr <- paste("mdl <- lm(value ~ variable + factor(",idField,"),data=melt_Model_dta)", sep="")
                eval(parse(text=execstr))
                # mdl <- lm(value ~ variable + factor(idField), data=melt_Model_dta)

                print("bts3.0.2b2")

                # Apply the model to interpolate
                for (u in 1:length(years)) {

                    varI <- gsub('####', years[[u]], cur_ancVi)
                    if (!(varI %in% colnames(dta@data))) {
                        # Variable doesn't exist, so we need to interpolate.
                        tDframe[idField] <- dta@data[idField]
                        tDframe["variable"] <- years[[u]]
                        dta@data[varI] <- predict(mdl, newdata=tDframe)

                    }
                }
            }

        }

        print("bts3.1")        
        # Append interpolated fields to our melting lists
        for (v in 1:length(interpYears)) {
            varList_pre[[length(varList_pre)+1]] <- interpYears[v]
        }
    
    }
  
    timer <- proc.time() - timer
    print(paste("section completed in", timer[3], "seconds."))



    print("bts4")
    timer <- proc.time()

    # Run the melts
    meltList <- list()
    for (i in 1:length(varList_pre)) {


        # Limit to only relevant years
        grepStrYrs = idField

        for (j in 1:length(years)) {
            tempGrep <- grepStrYrs

            if (regexpr("####", varList_pre[[i]], fixed=TRUE)[1] == -1) {
                grepStrYrs <- paste(tempGrep,"|",paste(varList_pre[[i]],years[[j]], sep="_"), sep="")
            } else {
                grepStrYrs <- paste(tempGrep,"|",gsub('####', years[[j]], varList_pre[[i]]), sep="")
            }
        }
    


        tDF <- dta@data[grepl(grepStrYrs, names(dta@data))]
        meltList[[i]] <- melt(tDF, id=idField)
        
        # Keep only years in the year column, rename columns
        colnames(meltList[[i]])[2] <- "Year"
        colnames(meltList[[i]])[3] <- varList_pre[[i]]
        
        # Clean up year column
        gsub_command <- paste("^",varList_pre[[i]],sep="")
        meltList[[i]][2] <- gsub(gsub_command, "", as.matrix(meltList[[i]][2]))
        
    
        # Remove ID and year if this is at least the second variable to avoid duplications.
        if (i > 1) {
            meltList[[i]] <- meltList[[i]][3]
        }

    }

    timer <- proc.time() - timer
    print(paste("section completed in", timer[3], "seconds."))


    # Finish up with a cherry on top
    meltListRet <- data.frame(meltList)
  
    return(meltListRet)
}


# dm1 <- melt(d[,c("Type","I.alt","idx06","idx07","idx08")], id=c("Type","I.alt"))
# dm2 <- melt(d[,c("Type","I.alt","farve1","farve2")], id=c("Type","I.alt"))
# colnames(dm2) <- c("Type", "I.alt", "variable2", "value2")
# dm <- merge(dm1, dm2)
