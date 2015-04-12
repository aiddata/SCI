#Date Started: 4/4/2015
#Contact: dan@danrunfola.com
#Description:
#This script runs a number of functions designed to support a *cross-sectional* spatial impact evaluation.
#Specifically, it seeks to examine the impact of a treatment that is applied spatially to some units, but not others.
#It is heavily limited in functionality at this stage.
#Steps this and related functions support:
#(1) Fitting a model for a propensity score match.
#(2) Conductnig a matching of pairs based on these matches.
#(3) Checking the balance of these pairs
#(4) Automated iteration on pairwise-balance to achieve the optimal balance given data and parameters.
#(5) Analysis of the final model and treatment effect, given matched data.

#The roadmap for this script includes better diagnostics for assumptions,
#Handling spatial relationships in data (i.e., spillovers),
#and more options for matching approaches.

#It requires as inputs:
#(A) A spatial data frame object (library sp), i.e. one generated from a shapefile.
#(B) A column identified as a binary treatment
#(C) A column identified as a continious outcome
#(D) A list of columns identifying covariates to include in the matching procedure
#(E) A list of columns identifying covariates to include in the analysis procedure


#Load required libraries from our dependencies script.
source('Dependencies/dep.R', chdir=T)

#Load descriptive tools
source('Tools/descriptives.R', chdir=T)

source('Tools/SpatialCausalPSM.R', chdir=T)

#Custom Functions
source('functions.R', chdir=T)

#--------------------------------
#Tool runs
#Load the Shapefile
user_shape <- "/home/aiddata/Desktop/R_Repo/C-SAT/Input_Data/User_Dan/KFW_poly.shp"
src_Shp = readShapePoly(user_shape)

#----------------------
#----------------------
#Data pre-processing.  This will be different for each script, and is here only as an illustrative example.
#Here, we create a binary for the treatment, as well as create a few baseline measurements for cross-sectional analysis.

#Pre-trend for NDVI
src_Shp@data["NDVI_trend"] <- src_Shp@data["NDVI_1995"] - src_Shp@data["NDVI_1981"]

#Final NDVI Change Value
src_Shp@data["NDVI_outcome"] <- src_Shp@data["NDVI_2013"] - src_Shp@data["NDVI_1995"]

#Binary for the Treatment
src_Shp@data["TrtBin"] <- 0
src_Shp@data["TrtBin"][src_Shp@data["Accepted_Y"] > 1998] <- 1

#----------------------------
#----------------------------
#Define the PSM equation, used in the modules below:
PSM_eq = "TrtBin ~ NDVI_trend + NDVI_1995 + CommunityA + factor(State)"

#Define the analysis equation, used in the final step:
Final_eq = "NDVI_outcome ~ TrtBin + NDVI_trend + NDVI_1995 + CommunityA + factor(State)"

#Function takes in an equation for the PSM, options on the type of PSM to return values for, and returns a new vector
#of PSM estimates.  data is the datset, method can only be "logit" at this time, equ is the PSM equation to estimate, 
#"rem" removes PSMs that do not have value overlap between the treatment / control groups based on min/max values.
#Returns a shapefile with a new column - PSM_trtProb.
psm_Res <- SpatialCausalPSM(dta = src_Shp, mtd = "logit", PSM_eq, drop="overlap")

#Here, we conduct the matching based on the PSM.
#An optimization algorithm is used to select optimal nearest neighbors.
#This function returns a shapefile with matched neighbors, contained in a new column - PSM_pairs.
psm_Pairs <- SpatialCausalDist(dta = psm_Res, mtd = "fastNN", vars = PSM_eq, ids = "reu_id", drop_unmatched = TRUE, drop_method = "SD", drop_thresh=.25)

#Run any set of models you are interested in, and save them in a one-dimensional array.
SpatialCausal_Model = vector()
SpatialCausal_Model[[1]] = lm("NDVI_outcome ~ TrtBin + NDVI_trend + NDVI_1995 + CommunityA + factor(State)", psm_Pairs)
SpatialCausal_Model[[2]] = lm("NDVI_outcome ~ TrtBin + NDVI_trend + NDVI_1995 + log(CommunityA) + factor(State)", psm_Pairs)

#Function to compare any 2 models in terms of their estimation on treatment effect.
#In our case, these are generally pre-defined models (i.e., models from a DGP)
#This function returns a Moran's I measurement of spatial autocorrelation in the outcome, treatment, and average of all controls.
#It also returns the difference in beta coefficients on the treatment for each model.


