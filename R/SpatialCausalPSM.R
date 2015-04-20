SpatialCausalPSM <- function(dta, mtd,mdl,drop, visual)
{
  #Initialization
  pltObjs <- list()
  
  #Method
  if(mtd == "logit")
  {
    PSMfit <- glm(mdl, dta@data, family="binomial")
    retData <- dta
    retData$PSM_trtProb <- predict(PSMfit, dta@data, type="response")
  }
  if(mtd=="lm")
  {
    PSMfit <- lm(mdl, dta@data)
    retData <- dta
    retData$PSM_trtProb <- predict(PSMfit, dta@data, type="response")
  }
  
  if(visual == "TRUE")
  {
    #Show user distributions.
    pltObjs[[1]] <- GroupCompHist(retData, "PSM_trtProb","Initial PSM Balance",simple_out=FALSE)
    print(summary(PSMfit))
  }

  
  #Second, if a drop parameter - if set to "overlap", remove observations
  #that don't overlap in the PSM distribution.
  if(drop == "overlap")
  {
    
    #Drop
    treated <- retData@data[retData@data$TrtBin == 1,]
    untreated <- retData@data[retData@data$TrtBin == 0,]
    min_cut <- max(min(treated$PSM_trtProb), min(untreated$PSM_trtProb))
    max_cut <- min(max(treated$PSM_trtProb), max(untreated$PSM_trtProb))
    
    retData <- retData[retData@data$PSM_trtProb >= min_cut,]    
    retData <- retData[retData@data$PSM_trtProb <= max_cut,] 
    
    if(visual == "TRUE")
    {
    #Post drop histograms
    pltObjs[[2]] <- GroupCompHist(retData, "PSM_trtProb","Post-Extrapolation Drops",simple_out=FALSE)
      
    #Output graphics
    grid.arrange(pltObjs[[1]], pltObjs[[2]],ncol=2,main="PSM Matching Stage 1 (Dropping Observations Requiring Extrapolation)")
    
    }
  }
  return (retData)
}

SpatialCausalDist <- function(dta, mtd, vars, ids, drop_unmatched, drop_method, drop_thresh, visual)
{
  #Initialization
  pltObjs <- list()
  init_dta <- dta
  
  if (mtd == "fastNN")
  {
    #Fast nearest neighbors search - will not arrive at optimum,
    #but this may not be an issue for many analysis.
    #Effectively loops through all observations in the treatment group, ordered by PSM score - higher scores go first.
    
    sorted_dta <- dta@data[order(dta@data$PSM_trtProb),]
    #Conduct the matching
    
    treated <- sorted_dta[sorted_dta$TrtBin == 1,]
    untreated <- sorted_dta[sorted_dta$TrtBin == 0,]
    
    it_cnt = min(length(treated[[1]]), length(untreated[[1]]))
    dta@data$match <- -999
    dta@data$PSM_distance <- -999
    dta@data$PSM_match_ID <- -999
    it_cnt = it_cnt - 1

    for (j in 1:it_cnt)
    {
      treated <- sorted_dta[sorted_dta$TrtBin == 1,]
      untreated <- sorted_dta[sorted_dta$TrtBin == 0,]
      
    
      #Run the KNN for all neighbors. 

      k <- get.knnx(treated$PSM_trtProb, untreated$PSM_trtProb, 1)

      #Add the matched treatment and control values to the recording data frame
      #best_m_control is the row in the "distance" matrix with the lowest value.  This is the same row as in the index.
      best_m_control = which(k$nn.dist %in% sort(k$nn.dist)[1])
      
      #This will give us the matched index in the "treated" dataset.
      best_m_treated = k$nn.index[best_m_control]
      
      #Control PSM ID
      cid_txt = paste("untreated$",ids,"[",best_m_control,"]",sep="")
      Control_ID = toString(eval(parse(text=cid_txt)))
      
      
      
      #Treatment PSM ID
      tid_txt = paste("treated$",ids,"[",best_m_treated,"]",sep="")
      Treatment_ID = toString(eval(parse(text=tid_txt)))
       
      #Add the Treatment ID to the Control Row 
      tid_a_1 = paste("dta@data$match[which(dta@data$",ids," == Control_ID)] = Treatment_ID", sep="")
      tid_a_2 = paste("dta@data$PSM_distance[which(dta@data$",ids," == Control_ID)] = k$nn.dist[,1][best_m_control]",sep="")
      tid_a_3 = paste("dta@data$PSM_match_ID[which(dta@data$",ids," == Control_ID)] = j", sep="")
      eval(parse(text=tid_a_1))
      eval(parse(text=tid_a_2))
      eval(parse(text=tid_a_3))
      
      
       
      #Add the Control ID to the Treatment Row
      cid_a_1 = paste("dta@data$match[which(dta@data$",ids," == Treatment_ID)] = Control_ID", sep="")
      cid_a_2 = paste("dta@data$PSM_distance[which(dta@data$",ids," == Treatment_ID)] = k$nn.dist[,1][best_m_control]", sep="")
      cid_a_3 = paste("dta@data$PSM_match_ID[which(dta@data$",ids," == Treatment_ID)] = j", sep="")
      eval(parse(text=cid_a_1))
      eval(parse(text=cid_a_2))
      eval(parse(text=cid_a_3))
      
       
      #Drop the paired match out of the iteration matrix 
      did_a_1 = paste("sorted_dta <- sorted_dta[sorted_dta$",ids,"!= Treatment_ID ,]",sep="")
      did_a_2 = paste("sorted_dta <- sorted_dta[sorted_dta$",ids,"!= Control_ID ,]",sep="")
      eval(parse(text=did_a_1))
      eval(parse(text=did_a_2))

     
    }
  }

  if (mtd == "optNN")
  {
    print("To contain a nearest-neighbor algorithm...")
  }

  if (drop_unmatched == TRUE)
  {
    dta <- dta[dta@data$PSM_match_ID != -999,]    
  }
    
  anc_v_int <- strsplit(vars, "~")[[1]][2]
  anc_vars <- strsplit(gsub(" ","",anc_v_int), "+", fixed=TRUE)
  anc_vars <- c(anc_vars[[1]], "PSM_trtProb")
  
  #Drop observations according to the selected method
  if(drop_method == "SD")
  {
    #Method to drop pairs that are greater than a set threshold apart in terms of PSM Standard Deviations.
    psm_sd_thresh = sd(dta$PSM_trtProb) * drop_thresh
    if(visual == "TRUE")
    {
      print(psm_sd_thresh)
    }
    dta <- dta[dta@data$PSM_distance < psm_sd_thresh,]
  }

  #Plot the pre and post-dropping balance for PSM model...
  for (i in 1:length(anc_vars))
  {
    #gsub to remove any factors()
    ed_v = sub("factor\\(","",anc_vars[i])
    ed_v = sub(")","",ed_v)
    db_i = paste("print(describeBy(init_dta@data$",ed_v,", group=init_dta@data$TrtBin))")  
    db_p = paste("print(describeBy(dta@data$",ed_v,", group=dta@data$TrtBin))") 
    c_type = eval(parse(text=paste("class(init_dta@data$",ed_v,")")))
    if((c_type == "numeric") & (visual == "TRUE"))
    {
     pltObjs[[length(pltObjs) + 1]] <- GroupCompHist(init_dta, anc_vars[i],"Pre-Balancing: ",simple_out = FALSE)
     pltObjs[[length(pltObjs) + 1]] <- GroupCompHist(dta, anc_vars[i],"Post-Balancing: ",simple_out = FALSE)  
     print("")
     print("=====================")
     print("=====================")
     print(ed_v)
     print("::::::::::::Pre-Matching Balance:::::::::::::::")
     eval(parse(text=db_i))
     print("")
     print("::::::::::::Post-Matching Balance:::::::::::::::")
     eval(parse(text=db_p))
    }
  }

    if(visual=="TRUE")
    {
      #Output graphics
      #Remove the factor rows
      nrow_c <- length(pltObjs)
      counter <- 1
      while(counter <= nrow_c)
      {
      d = counter + 3
      if(d > nrow_c)
      {
        d = nrow_c
      }
      do.call(grid.arrange,c(pltObjs[counter:d],nrow=2,ncol=2))
      counter = counter + 4
      }
    }
  
  
  
  return (dta)
}


SpatialCausalSim_DGP<- function(fld_size, SpatialCov_opt,rho_mult)
{
  x <- fld_size
  RFoptions(seed=NA)
  
  model <- RMexp()
  z <- RFsimulate(model, x, x, n=6)
  f.SPDF <- as(z, 'SpatialPolygonsDataFrame')
  
  colnames(f.SPDF@data)[1] <- "ControlA"
  colnames(f.SPDF@data)[2] <- "RandomFieldA"
  colnames(f.SPDF@data)[3] <- "ControlB"
  colnames(f.SPDF@data)[4] <- "RandomFieldB"
  colnames(f.SPDF@data)[5] <- "TrtBin"
  colnames(f.SPDF@data)[6] <- "tmpLag"
  
  #Amplify spatial correlation to an arbitrary degree in each dataset.
  #This is to account for the issue that gaussian random fields are highly unlikely to have
  #Extreme values of spatial correlation.
  f.NB = poly2nb(f.SPDF)
  f.W = nb2listw(f.NB, style='W')

  for (i in 1:5)
  {
    if(grepl(colnames(f.SPDF@data)[i],SpatialCov_opt) == 1)
    {
      rho = runif(1,0.1,(1*rho_mult))
      ev_st = paste("eleLag <- lag.listw(f.W, f.SPDF@data[[",i,"]])",sep="")
      eval(parse(text=ev_st))
      f.SPDF@data["tmpLag"]=data.frame(eleLag)
      ev_stB = paste("f.SPDF@data[",i,"] = scale((f.SPDF@data['tmpLag']*rho) + f.SPDF@data[",i,"])",sep="")
      eval(parse(text=ev_stB)) 
    } else {
      ev_stC = paste("f.SPDF@data[",i,"] = rnorm(nrow(f.SPDF))",sep="")
      eval(parse(text=ev_stC))
    }
    
  }
  
  f.SPDF@data["TrtBin"] = f.SPDF@data["ControlA"] + f.SPDF@data["RandomFieldA"]
  #Convert the Treatment to a Binary
  #Create a temporary field
  f.SPDF@data$TrtTemp <- f.SPDF@data$TrtBin
  med_treat = median(f.SPDF@data$TrtTemp)
  f.SPDF@data$TrtBin[which(f.SPDF@data$TrtTemp >= med_treat )] <- 1 
  f.SPDF@data$TrtBin[which(f.SPDF@data$TrtTemp < med_treat )] <- 0 
  f.SPDF@data <- f.SPDF@data[,!names(f.SPDF) %in% c("TrtTemp")]
  
  #Build outcome variable for testing
  #y = intercept + (Theta * Treatment) + (Beta * ControlA)
  yfunc <- function(a, b, c) (0.0+(1.0*a)+(1.0*b)+abs(1.0*c))
  f.SPDF@data["y"] = apply(f.SPDF@data[,c('TrtBin','ControlA','RandomFieldB')], 1, function(y) yfunc(y['TrtBin'],y['ControlA'],y['RandomFieldB']))

  f.SPDF@data <- cbind(simIDs = rownames(f.SPDF@data), f.SPDF@data)
  
  return(f.SPDF)
}