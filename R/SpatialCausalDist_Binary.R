SpatialCausalDist_Binary <- function(dta, mtd, constraints, psm_eq, ids, drop_opts, visual, TrtBinColName)
{
  #Initialization
  pltObjs <- list()
  init_dta <- dta
  
  drop_unmatched = drop_opts["drop_unmatched"]
  drop_method = drop_opts["drop_method"]
  drop_thresh = drop_opts["drop_thresh"]
  
  if(!is.null(constraints))
  {
    exec_stmnt = paste("dta$ConstraintGroupSet_Opt <- dta$",constraints["groups"],sep="")
    eval(parse(text=exec_stmnt))
  } else {
    dta$ConstraintGroupSet_Opt <- 1
  }
  
  #Caclulate the number of groups to constrain by, if any.
  group_constraints <- unique(dta$ConstraintGroupSet_Opt)
  
  #If there are more than 1 group, make sure they have at least one observation in the treatment and control groups.
  for (grp in 1:length(group_constraints))
  {
    cur_grp <- as.matrix(group_constraints)[grp]
    t_dta <- dta[dta$TrtBin == 1,]
    u_dta <- dta[dta$TrtBin == 0,]
    print(cur_grp)
    treatment_count <- cur_grp %in% t_dta$ConstraintGroupSet_Opt
    untreated_count <- cur_grp %in% u_dta$ConstraintGroupSet_Opt
    if((untreated_count == FALSE) || (treatment_count == FALSE))
    {
      dta <- dta[!dta$ConstraintGroupSet_Opt == cur_grp,]
      war_statement = paste("Dropped group due to a lack of both treatment and control observation: ",cur_grp,sep="")
      warning(war_statement)
    }
  }

  if (mtd == "fastNN")
  {
    dta <- fastNN_binary_func(dta,TrtBinColName,ids) 
  }
  
  if (mtd == "optNN")
  {
    print("To contain a nearest-neighbor algorithm...")
  }
  
  if (mtd == "NN_WithReplacement")
  {
    dta <- NN_WithReplacement_binary_func(dta,TrtBinColName,ids) 
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
    db_i = paste("print(describeBy(init_dta@data$",ed_v,", group=init_dta@data$",TrtBinColName,"))")  
    db_p = paste("print(describeBy(dta@data$",ed_v,", group=dta@data$",TrtBinColName,"))") 
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