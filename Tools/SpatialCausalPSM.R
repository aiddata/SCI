SpatialCausalPSM <- function(dta, mtd,mdl,drop)
{
  #First, we check for methods to fit the PSM.
  if(mtd == "logit")
  {
    PSMfit <- glm(mdl, dta@data, family="binomial")
    retData <- dta
    retData$PSM_trtProb <- predict(PSMfit, dta@data, type="response")
  }
  
  #Show user distributions.
  GroupCompHist(retData, "PSM_trtProb","Initial")
  
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
    
    #Post drop histograms
    GroupCompHist(retData, "PSM_trtProb","Post Extrapolation Drops")
  }
  
  return (retData)
}

SpatialCausalDist <- function(dta, mtd, vars, ids, drop_unmatched, drop_method, drop_thresh)
{
  #Save initial data for later comparison
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
    dta@data$match <- -1
    dta@data$PSM_distance <- -1
    dta@data$PSM_match_ID <- -1
    it_cnt = it_cnt - 1
    for (j in 1:it_cnt)
    {
      treated <- sorted_dta[sorted_dta$TrtBin == 1,]
      untreated <- sorted_dta[sorted_dta$TrtBin == 0,]
      
    
      #Run the KNN for all neighbors. 
      k <- get.knnx(treated$PSM_trtProb, untreated$PSM_trtProb, 2)

      #Add the matched treatment and control values to the recording data frame
      best_m = as.matrix(apply(k$nn.dist, 2, which.min))[2]


      #Control PSM ID
      cid_txt = paste("untreated$",ids,"[as.matrix(apply(k$nn.dist, 2, which.min))[2]]",sep="")
      Control_ID = eval(parse(text=cid_txt))

      
      
      #Treatment PSM ID
      k_match_id = k[[1]][best_m,][2]
      tid_txt = paste("treated$",ids,"[k_match_id]",sep="")
      Treatment_ID = eval(parse(text=tid_txt))

       
      #Add the Treatment ID to the Control Row 
      tid_a_1 = paste("dta@data$match[which(dta@data$",ids," == Control_ID)] = Treatment_ID", sep="")
      tid_a_2 = paste("dta@data$PSM_distance[which(dta@data$",ids," == Control_ID)] = k$nn.dist[,2][k_match_id]",sep="")
      tid_a_3 = paste("dta@data$PSM_match_ID[which(dta@data$",ids," == Control_ID)] = j", sep="")
      eval(parse(text=tid_a_1))
      eval(parse(text=tid_a_2))
      eval(parse(text=tid_a_3))
      
      
       
      #Add the Control ID to the Treatment Row
      cid_a_1 = paste("dta@data$match[which(dta@data$",ids," == Treatment_ID)] = Control_ID", sep="")
      cid_a_2 = paste("dta@data$PSM_distance[which(dta@data$",ids," == Treatment_ID)] = k$nn.dist[,2][k_match_id]", sep="")
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
    dta <- dta[dta@data$PSM_match_ID != -1,]    
  }
  
  anc_v_int <- strsplit(vars, "~")[[1]][2]
  anc_vars <- strsplit(gsub(" ","",anc_v_int), "+", fixed=TRUE)
  anc_vars <- c(anc_vars[[1]], "PSM_trtProb")
  
  #Drop observations according to the selected method
 
  if(drop_method == "SD")
  {
    #Method to drop pairs that are greater than a set threshold apart in terms of PSM Standard Deviations.
    psm_sd_thresh = sd(dta$PSM_trtProb) * drop_thresh
    print(psm_sd_thresh)
    dta <- dta[dta@data$PSM_distance < psm_sd_thresh,]
  }

  #Plot the pre and post-dropping balance for PSM model...
  for (i in 1:length(anc_vars))
  {
    GroupCompHist(init_dta, anc_vars[i],"Pre-Balancing: ")
    GroupCompHist(dta, anc_vars[i],"Post-Balancing: ")  
    #gsub to remove any factors()
    ed_v = sub("factor\\(","",anc_vars[i])
    ed_v = sub(")","",ed_v)
    db_i = paste("print(describeBy(init_dta@data$",ed_v,", group=init_dta@data$TrtBin))")  
    db_p = paste("print(describeBy(dta@data$",ed_v,", group=dta@data$TrtBin))") 
    c_type = eval(parse(text=paste("class(init_dta@data$",ed_v,")")))
    if(c_type == "numeric")
    {
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

  
  
  
  return (dta)
}