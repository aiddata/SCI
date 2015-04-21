SpatialCausalDist_Binary <- function(dta, mtd, psm_eq, ids, drop_opts, visual)
{
  #Initialization
  pltObjs <- list()
  init_dta <- dta
  
  #Set defaults
  drop_unmatched = FALSE
  drop_method = "None"
  drop_thresh = 0.25 #Ignored by default
  
  drop_unmatched = drop_opts["drop_unmatched"]
  drop_method = drop_opts["drop_method"]
  drop_thresh = drop_opts["drop_thresh"]

  
  if (mtd == "fastNN")
  {
    dta <- fastNN_binary_func(dta,"TrtBin",ids) 
  }
  
  if (mtd == "optNN")
  {
    print("To contain a nearest-neighbor algorithm...")
  }
  
  if (mtd == "NN_WithReplacement")
  {
    dta <- NN_WithReplacement_binary_func(dta,"TrtBin",ids) 
  }
  
  if (drop_unmatched == TRUE)
  {
    dta <- dta[dta@data$PSM_match_ID != -999,]    
  }
  
  anc_v_int <- strsplit(psm_eq, "~")[[1]][2]
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