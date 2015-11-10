#FastNN 
#Algorithm to find a hopefully near-optimal match of pairs
#In a treatment and control group
#Works by first ordering by the propensity score matching value,
#and then working through this list in order from highest to lowest.
#Matches are removed each step.

fastNN_binary_func <- function(dta, trtMntVar, ids, curgrp, dist_PSM) {

    print("nn1.0")
    # timerx <- proc.time()


    #Fast nearest neighbors search - will not arrive at optimum,
    #but this may not be an issue for many analysis.
    #Effectively loops through all observations in the treatment group, ordered by PSM score - higher scores go first.

    sorted_dta <- as.data.table(dta@data)

    print("x")
    print(colnames(sorted_dta))
    sorted_dta <- sorted_dta[, c(ids, trtMntVar, "PSM_trtProb"), with=FALSE]
    print(colnames(sorted_dta))

    sorted_dta <- sorted_dta[order(sorted_dta$PSM_trtProb)]
    print(colnames(sorted_dta))




    #Conduct the matching
    treated <- sorted_dta[get(trtMntVar) == 1]
    untreated <- sorted_dta[get(trtMntVar) == 0]

    it_cnt = min(length(treated[[1]]), length(untreated[[1]]))
    # dta@data[["match"]] <- -999
    # dta@data[["PSM_distance"]] <- -999
    dta@data[["PSM_match_ID"]] <- -999

    print("nn2")

    #Calculate a distance decay function
    #to perturb pairs based on their distances.  
    for (j in 1:it_cnt) {
        # time_list <- c()

        print("nn2.0")
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

        k <- get.knnx(treated[, PSM_trtProb], untreated[, PSM_trtProb], 1)
        
        # time_list[2] <- round((proc.time() - timer)[3],5)
        print("nn2.2")
        # timer <- proc.time()

        #Perturb the values based on the distance decay function, if selected.
        if (!is.null(dist_PSM)) {
            for (mC in 1:length(k[[1]])) {

                print("nn2.2.0")

                #Calculate the Euclidean Distance between pairs
                Control_ID = toString(untreated[mC, get(ids)])

                mT = k[["nn.index"]][mC]
                
                Treatment_ID = toString(treated[mT, get(ids)])

                #Find the control x,y location
                cCoord = coordinates(dta[which(dta@data[[ids]] == Control_ID),])
                

                #Find the treatment x,y location
                tCoord = coordinates(dta[which(dta@data[[ids]] == Treatment_ID),])

                y_dist = abs(cCoord[1] - cCoord[2])
                x_dist = abs(tCoord[1] - tCoord[2])
                euc_dist = sqrt(y_dist^2 + x_dist^2)
                
                print("nn2.2.1")

                PSM_score = k[["nn.dist"]][mC]
                geog_Weight = pairDistWeight(dist=euc_dist,threshold=dist_PSM,type="Spherical")

                print("nn2.2.2")

                
                k[["nn.dist"]][mC] <- ((1-geog_Weight) * PSM_score)

            }
      
        }

        # time_list[3] <- round((proc.time() - timer)[3],5)
        print("nn2.3")
        # timer <- proc.time()

        #Add the matched treatment and control values to the recording data frame
        #best_m_control is the row in the "distance" matrix with the lowest value.  This is the same row as in the index.
        best_m_control = which(k[["nn.dist"]] %in% sort(k[["nn.dist"]])[1])
        
        #This will give us the matched index in the "treated" dataset.
        best_m_treated = k[["nn.index"]][best_m_control]
        

        #Control and Treatment PSM ID
        # cid_txt = paste("untreated$",ids,"[",best_m_control,"]",sep="")
        # Control_ID = toString(eval(parse(text=cid_txt)))
            
        # tid_txt = paste("treated$",ids,"[",best_m_treated,"]",sep="")
        # Treatment_ID = toString(eval(parse(text=tid_txt)))


        #Control PSM ID and Treatment PSM ID
        Control_ID = toString(untreated[best_m_control, get(ids)])
        Treatment_ID = toString(treated[best_m_control, get(ids)])


        #Create a unique pair ID for each group (will simply append a "1" if only 1 group)
        pair_id = paste(curgrp,j, sep="")
        


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

        treated <- treated[get(ids) != (Treatment_ID)]
        untreated <- untreated[get(ids) != (Control_ID)]
        
        # time_list[6] <- round((proc.time() - timer)[3],5)
        # print(paste(time_list))

    }

    # print((proc.time() - timerx)[3])

    return(dta) 

}





# #FastNN 
# #Algorithm to find a hopefully near-optimal match of pairs
# #In a treatment and control group
# #Works by first ordering by the propensity score matching value,
# #and then working through this list in order from highest to lowest.
# #Matches are removed each step.

# fastNN_binary_func <- function(dta, trtMntVar, ids, curgrp, dist_PSM) {

#     print("nn1.0")
#     timerx <- proc.time()

    

#     #Fast nearest neighbors search - will not arrive at optimum,
#     #but this may not be an issue for many analysis.
#     #Effectively loops through all observations in the treatment group, ordered by PSM score - higher scores go first.

#     sorted_dta <- dta@data[order(dta@data[["PSM_trtProb"]]), c(ids, trtMntVar, "PSM_trtProb")]


#     #Conduct the matching
#     treated <- sorted_dta[sorted_dta[[trtMntVar]] == 1,]
#     untreated <- sorted_dta[sorted_dta[[trtMntVar]] == 0,]

#     it_cnt = min(length(treated[[1]]), length(untreated[[1]]))
#     # dta@data[["match"]] <- -999
#     # dta@data[["PSM_distance"]] <- -999
#     dta@data[["PSM_match_ID"]] <- -999

#     print("nn2")

#     #Calculate a distance decay function
#     #to perturb pairs based on their distances.  
#     for (j in 1:it_cnt) {
#         # time_list <- c()

#         print("nn2.0")
#         # timer <- proc.time()

#         # treated <- sorted_dta[which(sorted_dta[[trtMntVar]] == 1 & sorted_dta[['nn_matched']] == 0),]
#         # untreated <- sorted_dta[which(sorted_dta[[trtMntVar]] == 0 & sorted_dta[['nn_matched']] == 0),]
        
#         # print(nrow(sorted_dta[which(sorted_dta[['nn_matched']] == 0),]))

#         # time_list[1] <- round((proc.time() - timer)[3],5)
#         # print("nn2.1")
#         # timer <- proc.time()

#         #Run the KNN for all neighbors. 
#         # print(length(treated[[1]]))
#         # summary(treated[["PSM_trtProb"]])
#         # print(length(untreated[[1]]))
#         # summary(untreated[["PSM_trtProb"]])

#         k <- get.knnx(treated[["PSM_trtProb"]], untreated[["PSM_trtProb"]], 1)
        
#         # time_list[2] <- round((proc.time() - timer)[3],5)
#         print("nn2.2")
#         # timer <- proc.time()

#         #Perturb the values based on the distance decay function, if selected.
#         if (!is.null(dist_PSM)) {
#             for (mC in 1:length(k[[1]])) {

#                 print("nn2.2.0")

#                 #Calculate the Euclidean Distance between pairs
#                 Control_ID = toString(untreated[[ids]][[mC]])

#                 mT = k[["nn.index"]][mC]
                
#                 Treatment_ID = toString(treated[[ids]][[mT]])

#                 #Find the control x,y location
#                 cCoord = coordinates(dta[which(dta@data[[ids]] == Control_ID),])
                

#                 #Find the treatment x,y location
#                 tCoord = coordinates(dta[which(dta@data[[ids]] == Treatment_ID),])

#                 y_dist = abs(cCoord[1] - cCoord[2])
#                 x_dist = abs(tCoord[1] - tCoord[2])
#                 euc_dist = sqrt(y_dist^2 + x_dist^2)
                
#                 print("nn2.2.1")

#                 PSM_score = k[["nn.dist"]][mC]
#                 geog_Weight = pairDistWeight(dist=euc_dist,threshold=dist_PSM,type="Spherical")

#                 print("nn2.2.2")

                
#                 k[["nn.dist"]][mC] <- ((1-geog_Weight) * PSM_score)

#             }
      
#         }

#         # time_list[3] <- round((proc.time() - timer)[3],5)
#         print("nn2.3")
#         # timer <- proc.time()

#         #Add the matched treatment and control values to the recording data frame
#         #best_m_control is the row in the "distance" matrix with the lowest value.  This is the same row as in the index.
#         best_m_control = which(k[["nn.dist"]] %in% sort(k[["nn.dist"]])[1])
        
#         #This will give us the matched index in the "treated" dataset.
#         best_m_treated = k[["nn.index"]][best_m_control]
        

#         #Control PSM ID
#         cid_txt = paste("untreated$",ids,"[",best_m_control,"]",sep="")
#         Control_ID = toString(eval(parse(text=cid_txt)))
            
#         #Treatment PSM ID
#         tid_txt = paste("treated$",ids,"[",best_m_treated,"]",sep="")
#         Treatment_ID = toString(eval(parse(text=tid_txt)))

#         #Control PSM ID and Treatment PSM ID
#         # Control_ID = toString(untreated[,ids][best_m_control])
#         # Treatment_ID = toString(treated[,ids][best_m_treated])


#         #Create a unique pair ID for each group (will simply append a "1" if only 1 group)
#         pair_id = paste(curgrp,j, sep="")
        


#         # time_list[4] <- round((proc.time() - timer)[3],5)
#         print("nn2.4x")
#         # timer <- proc.time()

#         #Add the Treatment ID to the Control Row and Add the Control ID to the Treatment Row
#         # dta@data$match[which(dta@data[[ids]] == Control_ID)] <- Treatment_ID
#         # dta@data$match[which(dta@data[[ids]] == Treatment_ID)] <- Control_ID


#         # dta@data$PSM_distance[which(dta@data[[ids]] == Control_ID | dta@data[[ids]] == Treatment_ID)] <- k[["nn.dist"]][,1][best_m_control]
#         dta@data$PSM_match_ID[which(dta@data[[ids]] == Control_ID | dta@data[[ids]] == Treatment_ID)] <- pair_id        


        
#         # time_list[5] <- round((proc.time() - timer)[3],5)
#         # timer <- proc.time()

#         #Drop the paired match out of the iteration matrix 
#         # sorted_dta <- sorted_dta[sorted_dta[[ids]] != Treatment_ID ,]
#         # sorted_dta <- sorted_dta[sorted_dta[[ids]] != Control_ID ,]    
    
#         # sorted_dta[which(sorted_dta[[ids]] == Control_ID | sorted_dta[[ids]] == Treatment_ID),][['nn_matched']] <- 1

#         treated <- treated[which(treated[[ids]] != Treatment_ID),]
#         untreated <- untreated[which(untreated[[ids]] != Control_ID),]
        
#         # time_list[6] <- round((proc.time() - timer)[3],5)
#         # print(paste(time_list))

#     }

#     print((proc.time() - timerx)[3])

#     return(dta) 

# }

