# #FastNN 
# #Algorithm to find a hopefully near-optimal match of pairs
# #In a treatment and control group
# #Works by first ordering by the propensity score matching value,
# #and then working through this list in order from highest to lowest.
# #Matches are removed each step.

# fastNN_binary_func <- function(dta, trtMntVar, ids, curgrp, dist_PSM) {

#     print("nn1.0")

#     #Fast nearest neighbors search - will not arrive at optimum,
#     #but this may not be an issue for many analysis.
#     #Effectively loops through all observations in the treatment group, ordered by PSM score - higher scores go first.


#     sorted_dta <- dta@data[order(dta@data[["PSM_trtProb"]]), c(ids, trtMntVar, "PSM_trtProb")]

#     #Conduct the matching
#     treated <- as.data.table(sorted_dta[sorted_dta[[trtMntVar]] == 1, c(ids, "PSM_trtProb")])
#     untreated <- as.data.table(sorted_dta[sorted_dta[[trtMntVar]] == 0, c(ids, "PSM_trtProb")])



#     it_cnt = min(nrow(treated), nrow(untreated))
#     # dta@data[["match"]] <- -999
#     # dta@data[["PSM_distance"]] <- -999
#     dta@data[["PSM_match_ID"]] <- -999

#     print("nn2")

#     # Calculate a distance decay function
#     # to perturb pairs based on their distances.  
#     for (j in 1:it_cnt) {

#         print("nn2.1")
      
#         k <- get.knnx(treated[, 'PSM_trtProb'], untreated[, 'PSM_trtProb'], 1)
        
#         # print("nn2.2")

#         # # Perturb the values based on the distance decay function, if selected.
#         # if (!is.null(dist_PSM)) {
#         #     for (mC in 1:length(k[[1]])) {

#         #         print("nn2.2.0")

#         #         # Calculate the Euclidean Distance between pairs
#         #         Control_ID = toString(untreated[mC, get(ids), with=FALSE])

#         #         mT = k[["nn.index"]][mC]
                
#         #         Treatment_ID = toString(treated[mT, get(ids), with=FALSE])

#         #         #Find the control x,y location
#         #         cCoord = coordinates(dta[which(dta@data[[ids]] == Control_ID),])
                

#         #         #Find the treatment x,y location
#         #         tCoord = coordinates(dta[which(dta@data[[ids]] == Treatment_ID),])

#         #         y_dist = abs(cCoord[1] - cCoord[2])
#         #         x_dist = abs(tCoord[1] - tCoord[2])
#         #         euc_dist = sqrt(y_dist^2 + x_dist^2)
                
#         #         print("nn2.2.1")

#         #         PSM_score = k[["nn.dist"]][mC]
#         #         geog_Weight = pairDistWeight(dist=euc_dist,threshold=dist_PSM,type="Spherical")

#         #         print("nn2.2.2")

                
#         #         k[["nn.dist"]][mC] <- ((1-geog_Weight) * PSM_score)

#         #     }
      
#         # }

#         print("nn2.3")

#         # Add the matched treatment and control values to the recording data frame
#         # best_m_control is the row in the "distance" matrix with the lowest value.  This is the same row as in the index.
#         best_m_control = which(k[["nn.dist"]] %in% sort(k[["nn.dist"]])[1])
        
#         # This will give us the matched index in the "treated" dataset.
#         best_m_treated = k[["nn.index"]][best_m_control]
        

#         # Control and Treatment PSM ID
#         # cid_txt = paste("untreated$",ids,"[",best_m_control,"]",sep="")
#         # Control_ID = toString(eval(parse(text=cid_txt)))
            
#         # tid_txt = paste("treated$",ids,"[",best_m_treated,"]",sep="")
#         # Treatment_ID = toString(eval(parse(text=tid_txt)))

#         print(class(untreated))
#         print(colnames(untreated))

#         # Control PSM ID and Treatment PSM ID
#         Control_ID = toString(untreated[best_m_control, (ids), with=FALSE])
#         Treatment_ID = toString(treated[best_m_control, (ids), with=FALSE])


#         # Create a unique pair ID for each group (will simply append a "1" if only 1 group)
#         if (is.null(curgrp)) {
#             pair_id <- paste('pair',j, sep="")
#         } else {
#             pair_id <- paste(curgrp,j, sep="")

#         }


#         print("nn2.4x")

#         #Add the Treatment ID to the Control Row and Add the Control ID to the Treatment Row
#         # dta@data$match[which(dta@data[[ids]] == Control_ID)] <- Treatment_ID
#         # dta@data$match[which(dta@data[[ids]] == Treatment_ID)] <- Control_ID


#         # dta@data$PSM_distance[which(dta@data[[ids]] == Control_ID | dta@data[[ids]] == Treatment_ID)] <- k[["nn.dist"]][,1][best_m_control]
#         dta@data$PSM_match_ID[which(dta@data[[ids]] == Control_ID | dta@data[[ids]] == Treatment_ID)] <- pair_id        


        
#         # Drop the paired match out of the iteration matrix 
#         # sorted_dta <- sorted_dta[sorted_dta[[ids]] != Treatment_ID ,]
#         # sorted_dta <- sorted_dta[sorted_dta[[ids]] != Control_ID ,]    
    
#         # sorted_dta[which(sorted_dta[[ids]] == Control_ID | sorted_dta[[ids]] == Treatment_ID),][['nn_matched']] <- 1

#         qt = quote(ids != Treatment_ID)
#         qu = quote(ids != Treatment_ID)

#         treated <- treated[eval(qt)]
#         untreated <- untreated[eval(qu)]
        


#     }

#     return(dta) 

# }













#FastNN 
#Algorithm to find a hopefully near-optimal match of pairs
#In a treatment and control group
#Works by first ordering by the propensity score matching value,
#and then working through this list in order from highest to lowest.
#Matches are removed each step.

fastNN_binary_func <- function(dta, trtMntVar, ids, curgrp, dist_PSM) {


    # print('nn')

    # # make sure there are at least x rows in both treated and control
    # row_min <- 30
    # trt_rows <- dta@data$TrtBin == 1
    # if ( sum(trt_rows) < row_min | sum(! trt_rows) < row_min ) {
    #     return('drop')
    # }



    # match_data <- dta@data[,c('PSM_trtProb', ids, 'TrtBin')]

    # rownames(match_data) <- match_data[[ids]]

    # zzz <<- match_data

    # m <- matchit(TrtBin ~ PSM_trtProb, data=match_data, method="nearest", ratio=1)

    # dta@data[["PSM_match_ID"]] <- -999

    # for ( i in rownames(m$match.matrix) ) {
    #     trt_id <- i
    #     cnt_id <- m$match.matrix[i,]
        
    #     if ( !is.na(cnt_id) ) {
    #         pair_id <- paste(trt_id,cnt_id, sep='__')
    #         dta@data$PSM_match_ID[which(dta@data[[ids]] == cnt_id | dta@data[[ids]] == trt_id)] <- pair_id        
    #     }
    # }


    # ===========================================================================
 

    print("nn1.0")


    #Fast nearest neighbors search - will not arrive at optimum,
    #but this may not be an issue for many analysis.
    #Effectively loops through all observations in the treatment group, ordered by PSM score - higher scores go first.

    sorted_dta <- dta@data[order(-dta@data[["PSM_trtProb"]]), c(ids, trtMntVar, "PSM_trtProb")]


    #Conduct the matching
    treated <- sorted_dta[sorted_dta[[trtMntVar]] == 1, c(ids, "PSM_trtProb")]
    untreated <- sorted_dta[sorted_dta[[trtMntVar]] == 0, c(ids, "PSM_trtProb")]

    it_cnt = min(length(treated[[1]]), length(untreated[[1]]))

    if (it_cnt < 30) {
        return('drop')
    }

    # dta@data[["match"]] <- -999
    # dta@data[["PSM_distance"]] <- -999
    dta@data[["PSM_match_ID"]] <- -999

    # print("nn2")

    #Calculate a distance decay function
    #to perturb pairs based on their distances.  
    for (j in 1:it_cnt) {
        # time_list <- c()

        print(paste("nn cnt:",j))
        # timer <- proc.time()

        # treated <- sorted_dta[which(sorted_dta[[trtMntVar]] == 1 & sorted_dta[['nn_matched']] == 0),]
        # untreated <- sorted_dta[which(sorted_dta[[trtMntVar]] == 0 & sorted_dta[['nn_matched']] == 0),]
        
        # print(nrow(sorted_dta[which(sorted_dta[['nn_matched']] == 0),]))

        # time_list[1] <- round((proc.time() - timer)[3],5)
        # print("nn2.1")
        # timer <- proc.time()

        #Run the KNN for all neighbors. 
        # print(length(treated[[1]]))
        # summary(treated[["PSM_trtProb"]])
        # print(length(untreated[[1]]))
        # summary(untreated[["PSM_trtProb"]])

        k <- get.knnx(treated[["PSM_trtProb"]], untreated[["PSM_trtProb"]], 1)
        
        # time_list[2] <- round((proc.time() - timer)[3],5)
        # print("nn2.2")
        # timer <- proc.time()

        # #Perturb the values based on the distance decay function, if selected.
        # if (!is.null(dist_PSM)) {
        #     for (mC in 1:length(k[[1]])) {

        #         print("nn2.2.0")

        #         #Calculate the Euclidean Distance between pairs
        #         Control_ID = toString(untreated[[ids]][[mC]])

        #         mT = k[["nn.index"]][mC]
                
        #         Treatment_ID = toString(treated[[ids]][[mT]])

        #         #Find the control x,y location
        #         cCoord = coordinates(dta[which(dta@data[[ids]] == Control_ID),])
                

        #         #Find the treatment x,y location
        #         tCoord = coordinates(dta[which(dta@data[[ids]] == Treatment_ID),])

        #         y_dist = abs(cCoord[1] - cCoord[2])
        #         x_dist = abs(tCoord[1] - tCoord[2])
        #         euc_dist = sqrt(y_dist^2 + x_dist^2)
                
        #         print("nn2.2.1")

        #         PSM_score = k[["nn.dist"]][mC]
        #         geog_Weight = pairDistWeight(dist=euc_dist,threshold=dist_PSM,type="Spherical")

        #         print("nn2.2.2")

                
        #         k[["nn.dist"]][mC] <- ((1-geog_Weight) * PSM_score)

        #     }
      
        # }

        # time_list[3] <- round((proc.time() - timer)[3],5)
        print("nn2.3")
        # timer <- proc.time()

        #Add the matched treatment and control values to the recording data frame
        #best_m_control is the row in the "distance" matrix with the lowest value.  This is the same row as in the index.
        best_m_control = which(k[["nn.dist"]] %in% sort(k[["nn.dist"]])[1])
        
        #This will give us the matched index in the "treated" dataset.
        best_m_treated = k[["nn.index"]][best_m_control]
        

        # #Control PSM ID
        cid_txt = paste("untreated$",ids,"[",best_m_control,"]",sep="")
        Control_ID1 = toString(eval(parse(text=cid_txt)))
            
        # #Treatment PSM ID
        tid_txt = paste("treated$",ids,"[",best_m_treated,"]",sep="")
        Treatment_ID1 = toString(eval(parse(text=tid_txt)))

        print(Control_ID1)
        print(Treatment_ID1)


        #Control PSM ID and Treatment PSM ID
        Control_ID = toString(untreated[[ids]][best_m_control])
        Treatment_ID = toString(treated[[ids]][best_m_treated])

        print(Control_ID)
        print(Treatment_ID)


        #Create a unique pair ID for each group (will simply append a "1" if only 1 group)
        # pair_id = paste(curgrp,j, sep="")
        pair_id <- paste(Treatment_ID,Control_ID, sep='__')
        print(pair_id)

        # time_list[4] <- round((proc.time() - timer)[3],5)
        print("nn2.4x")
        # timer <- proc.time()

        #Add the Treatment ID to the Control Row and Add the Control ID to the Treatment Row
        # dta@data$match[which(dta@data[[ids]] == Control_ID)] <- Treatment_ID
        # dta@data$match[which(dta@data[[ids]] == Treatment_ID)] <- Control_ID


        # dta@data$PSM_distance[which(dta@data[[ids]] == Control_ID | dta@data[[ids]] == Treatment_ID)] <- k[["nn.dist"]][,1][best_m_control]
        dta@data$PSM_match_ID[which(dta@data[[ids]] == Control_ID | dta@data[[ids]] == Treatment_ID)] <- pair_id        


        
        # time_list[5] <- round((proc.time() - timer)[3],5)
        # timer <- proc.time()

        #Drop the paired match out of the iteration matrix 
        # sorted_dta <- sorted_dta[sorted_dta[[ids]] != Treatment_ID ,]
        # sorted_dta <- sorted_dta[sorted_dta[[ids]] != Control_ID ,]    
    
        # sorted_dta[which(sorted_dta[[ids]] == Control_ID | sorted_dta[[ids]] == Treatment_ID),][['nn_matched']] <- 1

        treated <- treated[which(treated[[ids]] != Treatment_ID),]
        untreated <- untreated[which(untreated[[ids]] != Control_ID),]
        
        # time_list[6] <- round((proc.time() - timer)[3],5)
        # print(paste(time_list))

    }


    return(dta) 

}

