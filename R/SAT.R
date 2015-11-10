# SAT <- function (dta, mtd, constraints, psm_eq, ids, drop_opts, visual, TrtBinColName) {

#     dta <- as.data.table(dta@data)

#     #Initialization
#     pltObjs <- list()
#     init_dta <- dta
  
#     drop_unmatched = drop_opts["drop_unmatched"]
#     drop_method = drop_opts["drop_method"]
#     drop_thresh = as.numeric(drop_opts["drop_thresh"])
  
    

#     print("sat1")

#     if (!is.null(constraints) && contraints != c()) {
#         for (cst in 1:length(names(constraints))) {
#             if (names(constraints)[cst] == "groups") {
#                 dta[,"ConstraintGroupSet_Opt"] <- dta[,get(constraints["groups"])]

#             } else {
#                 dta[,"ConstraintGroupSet_Opt"] <- 1
#             }

#             if (names(constraints)[cst] == "distance") {
#                 dist_PSM = as.numeric(constraints["distance"][[1]])
#             } else {
#                 dist_PSM=NULL
#             }
#         }
        
#     } else {
#         dta[,"ConstraintGroupSet_Opt"] <- 1
#         #max the distance threshold by taking the diagonal of the bounding box.
#         dist_PSM = NULL
#     }



#     print("sat2")

#     #Caclulate the number of groups to constrain by, if any.
#     group_constraints <- unique(dta[,'ConstraintGroupSet_Opt']
  
#     #Make sure there are both treatment and control groups of an adequate size (>= 1 of each)
#     t_dta <- list()
#     u_dta <-list()
#     grp_list <- list()
#     cnt = 0

#     for (grp in 1:length(group_constraints)) {
#         cur_grp <- as.matrix(group_constraints)[grp]
#         grp_index = length(grp_list)+1
#         t_index = length(t_dta)+1
#         grp_list[[grp_index]] <- as.matrix(group_constraints)[grp]

#         t_dta[[t_index]] <- dta[TrtBin == 1]
#         u_dta[[t_index]] <- dta[TrtBin == 0]

#         has_treated <- cur_grp %in% t_dta[[t_index]][,'ConstraintGroupSet_Opt']
#         has_untreated <- cur_grp %in% u_dta[[t_index]][,'ConstraintGroupSet_Opt']

#         if ((has_untreated == FALSE) || (has_treated == FALSE)) {
#             dta <- dta['ConstraintGroupSet_Opt' != cur_grp]
#             t_dta[[t_index]] <- NULL
#             u_dta[[t_index]] <- NULL
#             grp_list[[t_index]] <- NULL
#             war_statement = paste("Dropped group due to a lack of both treatment and control observation: '",cur_grp,"'",sep="")
#             warning(war_statement)

#         } else { 
#             t_dta[[t_index]] <- t_dta[[t_index]][ConstraintGroupSet_Opt == (cur_grp)]
#             u_dta[[t_index]] <- u_dta[[t_index]][ConstraintGroupSet_Opt == (cur_grp)]

#             cnt = cnt + 1
#         }
#     }


#     if (cnt == 0) {
#         return('drop')
#     }


#     print("sat3")

#     temp_dta <- list()

#     for (i in 1:cnt) {
#         cur_grp <- grp_list[[i]]

#         print("sat3.1")
#         it_dta <- maptools::spRbind(t_dta[[i]],u_dta[[i]])

#         print("sat3.2")
#         if (mtd == "fastNN") {
#             # ***
#             # this is the slow part of functions
#             temp_dta[[i]] <- fastNN_binary_func(it_dta, TrtBinColName, ids, cur_grp, dist_PSM) 
#         }

#         if (mtd == "NN_WithReplacement") {
#             print("NN with replacement is currently not available, please choose fastNN")
#             # temp_dta[[i]] <- NN_WithReplacement_binary_func(it_dta,TrtBinColName,ids,cur_grp,dist_PSM) 
#         }
#     }

#     print("sat4")

#     #Build the final datasets from subsets
#     if (cnt > 1) {
#         dta <- temp_dta[[1]]
#         for(k in 2:cnt) {
#             dta  <- maptools::spRbind(dta, temp_dta[[k]])
#         } 
#     } else {
#         dta <- temp_dta[[1]]
#     }

#     print("sat5")

#     if (drop_unmatched == TRUE) {
#         dta <- dta["PSM_match_ID" != -999]    
#     }
  
#     anc_v_int <- strsplit(psm_eq, "~")[[1]][2]
#     anc_vars <- strsplit(gsub(" ","",anc_v_int), "+", fixed=TRUE)
#     anc_vars <- c(anc_vars[[1]], "PSM_trtProb")
    
#     print("sat6")
  
#     #Drop observations according to the selected method
#     if (drop_method == "SD") {
#         #Method to drop pairs that are greater than a set threshold apart in terms of PSM Standard Deviations.
#         psm_sd_thresh = sd(dta[,"PSM_trtProb"]) * drop_thresh
#         if (visual == "TRUE") {
#             print(psm_sd_thresh)
#         }
#         dta <- dta["PSM_distance" < psm_sd_thresh]
#     }
  


#     #Plot the pre and post-dropping balance for PSM model...
#     #Balance metrics are based on "Misunderstandings between experimentalists and
#     #observationalists about causal inference", Imal, King, and Stuart.
#     #Simplest suggestion of comparing means and checking if .25 SD apart used.
#     cnt = 0

#     print("sat7")
    
#     for (i in 1:length(anc_vars)) {

#         print("sat7.0")

#         #gsub to remove any factors()
#         ed_v = sub("factor\\(","",anc_vars[i])
#         ed_v = sub(")","",ed_v)

#         c_type = class(init_dta[[ed_v]])

#         print("sat7.1")
#         if (c_type == "matrix") {

#             dta[,ed_v] <- as.numeric(dta[,ed_v])

#             init_dta[,ed_v] <- as.numeric(init_dta[,ed_v])

#             c_type = "numeric"
#         }

#         print("sat7.2")
#         if ((c_type == "numeric") & (visual == "TRUE")) {
#             cnt = cnt + 1
#             pltObjs[[length(pltObjs) + 1]] <- GroupCompHist(init_dta, anc_vars[i],"Pre-Balancing: ",simple_out = FALSE)
#             pltObjs[[length(pltObjs) + 1]] <- GroupCompHist(dta, anc_vars[i],"Post-Balancing: ",simple_out = FALSE)  


#             treat_mean_pre <- round(describeBy(init_dta[[ed_v]], group=init_dta[[TrtBinColName]])[[2]][[3]], 5)
#             treat_SD_pre <- round(describeBy(init_dta[[ed_v]], group=init_dta[[TrtBinColName]])[[2]][[4]], 5)
#             control_mean_pre <- round(describeBy(init_dta[[ed_v]], group=init_dta[[TrtBinColName]])[[1]][[3]], 5)
#             control_SD_pre <- round(describeBy(init_dta[[ed_v]], group=init_dta[[TrtBinColName]])[[1]][[4]], 5)

#             treat_mean_post <- round(describeBy(dta[[ed_v]], group=dta[[TrtBinColName]])[[2]][[3]], 5)
#             treat_SD_post <- round(describeBy(dta[[ed_v]], group=dta[[TrtBinColName]])[[2]][[4]], 5)
#             control_mean_post <- round(describeBy(dta[[ed_v]], group=dta[[TrtBinColName]])[[1]][[3]], 5)
#             control_SD_post <- round(describeBy(dta[[ed_v]], group=dta[[TrtBinColName]])[[1]][[4]], 5)


#             it_diff_Mean_pre <- round(abs( treat_mean_pre-control_mean_pre ),5)
#             it_diff_Mean_post <- round(abs(treat_mean_post-control_mean_post),5)

#             if (!exists("bRes")) {

#                 bRes <- data.frame(treat_mean_pre,treat_SD_pre,control_mean_pre,control_SD_pre,
#                            treat_mean_post,treat_SD_post,control_mean_post,control_SD_post,
#                            it_diff_Mean_pre,it_diff_Mean_post)

#                 colnames(bRes)[1] <- "Pre-Balance Treated Mean"
#                 colnames(bRes)[2] <- "Pre-Balance Treated SD"
#                 colnames(bRes)[3] <- "Pre-Balance Control Mean"
#                 colnames(bRes)[4] <- "Pre-Balance Control SD"

#                 colnames(bRes)[5] <- "Post-Balance Treated Mean"
#                 colnames(bRes)[6] <- "Post-Balance Treated SD"
#                 colnames(bRes)[7] <- "Post-Balance Control Mean"
#                 colnames(bRes)[8] <- "Post-Balance Control SD"

#                 colnames(bRes)[9] <- "Mean Difference Pre-Balance"
#                 colnames(bRes)[10] <- "Mean Difference Post-Balance"

#             } else {
#                 bRes <- rbind(bRes, c(treat_mean_pre,treat_SD_pre,control_mean_pre,control_SD_pre,
#                               treat_mean_post,treat_SD_post,control_mean_post,control_SD_post,
#                               it_diff_Mean_pre,it_diff_Mean_post))
#             }
      
#             rownames(bRes)[i-(i-cnt)] <- gsub("[^a-zA-Z0-9]", "", ed_v)
#         }
#     }

#     print("sat8")
  
#     if (visual=="TRUE") {
#         #Output graphics
#         #Remove the factor rows
#         nrow_c <- length(pltObjs)
#         counter <- 1
#         while (counter <= nrow_c) {
#             d = counter + 3
#             if (d > nrow_c) {
#                 d = nrow_c
#             }
#             do.call(grid.arrange,c(pltObjs[counter:d],nrow=2,ncol=2))
#             counter = counter + 4
#         }
#         #bTab <- stargazer(bRes,summary=FALSE,type="html")
#         #print.htmlTable(bTab)
#     }
  

#     return (as.data.frame(dta))
# }





SAT <- function (dta, mtd, constraints, psm_eq, ids, drop_opts, visual, TrtBinColName) {
    #Initialization
    pltObjs <- list()
    init_dta <- dta
  
    drop_unmatched = drop_opts["drop_unmatched"]
    drop_method = drop_opts["drop_method"]
    drop_thresh = as.numeric(drop_opts["drop_thresh"])
  
    

    print("sat1")

    if (!is.null(constraints) && contraints != c()) {

        print("sat1a.1")

        for (cst in 1:length(names(constraints))) {
            if (names(constraints)[cst] == "groups") {
                dta@data[,"ConstraintGroupSet_Opt"] <- dta@data[,constraints["groups"]]

            } else {
                dta$ConstraintGroupSet_Opt <- 1
            }

            if (names(constraints)[cst] == "distance") {
                dist_PSM = as.numeric(constraints["distance"][[1]])
            } else {
                dist_PSM=NULL
            }
        }


        print("sat1a.2")

        # Caclulate the number of groups to constrain by, if any.
        group_constraints <- unique(dta$ConstraintGroupSet_Opt)
      
        # Make sure there are both treatment and control groups of an adequate size (>= 1 of each)
        t_dta <- list()
        u_dta <-list()
        grp_list <- list()
        cnt <- 0

        for (grp in 1:length(group_constraints)) {
            cur_grp <- as.matrix(group_constraints)[grp]
            grp_index = length(grp_list)+1
            t_index = length(t_dta)+1
            grp_list[[grp_index]] <- as.matrix(group_constraints)[grp]

            t_dta[[t_index]] <- dta[dta$TrtBin == 1,]
            u_dta[[t_index]] <- dta[dta$TrtBin == 0,]

            treatment_count <- cur_grp %in% t_dta[[t_index]]$ConstraintGroupSet_Opt
            untreated_count <- cur_grp %in% u_dta[[t_index]]$ConstraintGroupSet_Opt

            if ((untreated_count == FALSE) || (treatment_count == FALSE)) {
                dta <- dta[!dta$ConstraintGroupSet_Opt == cur_grp,]
                t_dta[[t_index]] <- NULL
                u_dta[[t_index]] <- NULL
                grp_list[[t_index]] <- NULL
                war_statement = paste("Dropped group due to a lack of both treatment and control observation: '",cur_grp,"'",sep="")
                warning(war_statement)

            } else { 
                t_dta[[t_index]] <- t_dta[[t_index]][t_dta[[t_index]]$ConstraintGroupSet_Opt == cur_grp,]
                u_dta[[t_index]] <- u_dta[[t_index]][u_dta[[t_index]]$ConstraintGroupSet_Opt == cur_grp,]

                cnt <- cnt + 1
            }
        }


        if (cnt == 0) {
            return('drop')
        }


        print("sat1a.3")


        for (i in 1:cnt) {
            cur_grp <- grp_list[[i]]

            print("sat1a.3.1")
            it_dta <- maptools::spRbind(t_dta[[i]],u_dta[[i]])

            print("sat1a.3.2")
            if (mtd == "fastNN") {
                # ***
                # this is the slow part of functions
                temp_dta[[i]] <- fastNN_binary_func(it_dta, TrtBinColName, ids, cur_grp, dist_PSM) 
            }

            # if (mtd == "NN_WithReplacement") {
            #     print("NN with replacement is currently not available, please choose fastNN")
            #     # temp_dta[[i]] <- NN_WithReplacement_binary_func(it_dta,TrtBinColName,ids,cur_grp,dist_PSM) 
            # }
        }

        print("sat1a.4")

        #Build the final datasets from subsets
        if (cnt > 1) {
            dta <- temp_dta[[1]]
            for(k in 2:cnt) {
                dta  <- maptools::spRbind(dta, temp_dta[[k]])
            } 
        } else {
            dta <- temp_dta[[1]]
        }

        
    } else {

        print("sat1b.1")

        cnt <- 1
        temp_dta <- list()

        if (mtd == "fastNN") {
            # ***
            # this is the slow part of functions
            dta <- fastNN_binary_func(dta, TrtBinColName, ids, NULL, NULL) 

            if (class(dta) == class('drop')) {
                return('drop')
            }
        }


        # if (mtd == "NN_WithReplacement") {
        #     print("NN with replacement is currently not available, please choose fastNN")
        #     # temp_dta[[i]] <- NN_WithReplacement_binary_func(it_dta,TrtBinColName,ids,cur_grp,dist_PSM) 
        # }


    }




    print("sat5")

    if (drop_unmatched == TRUE) {
        dta <- dta[dta@data[,"PSM_match_ID"] != -999,]    
    }
  
    anc_v_int <- strsplit(psm_eq, "~")[[1]][2]
    anc_vars <- strsplit(gsub(" ","",anc_v_int), "+", fixed=TRUE)
    anc_vars <- c(anc_vars[[1]], "PSM_trtProb")
    
    print("sat6")
  
    #Drop observations according to the selected method
    if (drop_method == "SD") {
        #Method to drop pairs that are greater than a set threshold apart in terms of PSM Standard Deviations.
        psm_sd_thresh = sd(dta@data[,"PSM_trtProb"]) * drop_thresh
        if (visual == "TRUE") {
            print(psm_sd_thresh)
        }
        dta <- dta[dta@data[,"PSM_distance"] < psm_sd_thresh,]
    }
  


    #Plot the pre and post-dropping balance for PSM model...
    #Balance metrics are based on "Misunderstandings between experimentalists and
    #observationalists about causal inference", Imal, King, and Stuart.
    #Simplest suggestion of comparing means and checking if .25 SD apart used.
    cnt = 0

    print("sat7")
    
    for (i in 1:length(anc_vars)) {

        print("sat7.0")

        #gsub to remove any factors()
        ed_v = sub("factor\\(","",anc_vars[i])
        ed_v = sub(")","",ed_v)

        c_type = class(init_dta@data[[ed_v]])

        print("sat7.1")
        if (c_type == "matrix") {

            dta@data[,ed_v] <- as.numeric(dta@data[,ed_v])

            init_dta@data[,ed_v] <- as.numeric(init_dta@data[,ed_v])

            c_type = "numeric"
        }

        print("sat7.2")
        if ((c_type == "numeric") & (visual == "TRUE")) {
            cnt = cnt + 1
            pltObjs[[length(pltObjs) + 1]] <- GroupCompHist(init_dta, anc_vars[i],"Pre-Balancing: ",simple_out = FALSE)
            pltObjs[[length(pltObjs) + 1]] <- GroupCompHist(dta, anc_vars[i],"Post-Balancing: ",simple_out = FALSE)  


            treat_mean_pre <- round(describeBy(init_dta@data[[ed_v]], group=init_dta@data[[TrtBinColName]])[[2]][[3]], 5)
            treat_SD_pre <- round(describeBy(init_dta@data[[ed_v]], group=init_dta@data[[TrtBinColName]])[[2]][[4]], 5)
            control_mean_pre <- round(describeBy(init_dta@data[[ed_v]], group=init_dta@data[[TrtBinColName]])[[1]][[3]], 5)
            control_SD_pre <- round(describeBy(init_dta@data[[ed_v]], group=init_dta@data[[TrtBinColName]])[[1]][[4]], 5)

            treat_mean_post <- round(describeBy(dta@data[[ed_v]], group=dta@data[[TrtBinColName]])[[2]][[3]], 5)
            treat_SD_post <- round(describeBy(dta@data[[ed_v]], group=dta@data[[TrtBinColName]])[[2]][[4]], 5)
            control_mean_post <- round(describeBy(dta@data[[ed_v]], group=dta@data[[TrtBinColName]])[[1]][[3]], 5)
            control_SD_post <- round(describeBy(dta@data[[ed_v]], group=dta@data[[TrtBinColName]])[[1]][[4]], 5)


            it_diff_Mean_pre <- round(abs( treat_mean_pre-control_mean_pre ),5)
            it_diff_Mean_post <- round(abs(treat_mean_post-control_mean_post),5)

            if (!exists("bRes")) {

                bRes <- data.frame(treat_mean_pre,treat_SD_pre,control_mean_pre,control_SD_pre,
                           treat_mean_post,treat_SD_post,control_mean_post,control_SD_post,
                           it_diff_Mean_pre,it_diff_Mean_post)

                colnames(bRes)[1] <- "Pre-Balance Treated Mean"
                colnames(bRes)[2] <- "Pre-Balance Treated SD"
                colnames(bRes)[3] <- "Pre-Balance Control Mean"
                colnames(bRes)[4] <- "Pre-Balance Control SD"

                colnames(bRes)[5] <- "Post-Balance Treated Mean"
                colnames(bRes)[6] <- "Post-Balance Treated SD"
                colnames(bRes)[7] <- "Post-Balance Control Mean"
                colnames(bRes)[8] <- "Post-Balance Control SD"

                colnames(bRes)[9] <- "Mean Difference Pre-Balance"
                colnames(bRes)[10] <- "Mean Difference Post-Balance"

            } else {
                bRes <- rbind(bRes, c(treat_mean_pre,treat_SD_pre,control_mean_pre,control_SD_pre,
                              treat_mean_post,treat_SD_post,control_mean_post,control_SD_post,
                              it_diff_Mean_pre,it_diff_Mean_post))
            }
      
            rownames(bRes)[i-(i-cnt)] <- gsub("[^a-zA-Z0-9]", "", ed_v)
        }
    }

    print("sat8")
  
    if (visual=="TRUE") {
        #Output graphics
        #Remove the factor rows
        nrow_c <- length(pltObjs)
        counter <- 1
        while (counter <= nrow_c) {
            d = counter + 3
            if (d > nrow_c) {
                d = nrow_c
            }
            do.call(grid.arrange,c(pltObjs[counter:d],nrow=2,ncol=2))
            counter = counter + 4
        }
        #bTab <- stargazer(bRes,summary=FALSE,type="html")
        #print.htmlTable(bTab)
    }
  

    return (dta)
}

