#FastNN 
#Algorithm to find a hopefully near-optimal match of pairs
#In a treatment and control group
#Works by first ordering by the propensity score matching value,
#and then working through this list in order from highest to lowest.
#Matches are removed each step.

fastNN_func <- function(dta,trtMntVar,ids)
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
  return(dta) 
}