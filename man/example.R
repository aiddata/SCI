#Example script for the use of the
#Spatial Causal Inference package in R
#Please note this package is in an early alpha release, and as such
#has many instabilities, bugs and errors, and is limited in functionality.
#If you encounter issues, do not hesitate to contact
#the authors at drunfola@aiddata.org .

#Load the SCI library using devtools
#Devtools is necessary as the package is currently
#only on Github

library(devtools)
devtools::install_github("itpir/SCI@master")
library(SCI)

#-------------------------------------------------
#-------------------------------------------------
#Load in a shapefile that has all of the columns 
#that might explain your outcome of interest.
#Here, we use an example of deforestation
#in the amazon.
#This example data can be viewed here:
#http://labs.aiddata.org/aiddata/kfw/
#-------------------------------------------------
#-------------------------------------------------
shpfile = file.path(getwd(),"man","data","example.shp")
dta_Shp = readShapePoly(shpfile)

#In this shapefile, each row is a community, 
#and each column is an associated measurement.
#The first function in this package enables users
#to collapse multiple-year columns
#into a single measurement.
#To look at a sample of the dataset, you can use either of these commands:
#plot(dta_Shp)
#View(dta_Shp)

#Here, we calculate the mean NDVI (a measure of vegetation
#cover ranging from 0-1, in which 0 indicates barren soil
#and 1 indicates dense vegetation) between 1982 and 1995 for
#each community, using SP_ID as a unique field.
#The MeanL_[0-9][0-9][0-9][0-9] is a search string that
#finds all columns of data that start with MeanL_ (the variable name),
#and also has a year in it (with each of the 4 year digits being equal to
#0-9).

dta_Shp$pre_trend_NDVI <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

#To see our new column of data:
#View(dta_Shp$pre_trend_NDVI_mean)
#Using the same function, calculate the trend after the intervention period
dta_Shp$NDVI_trend_01_10 <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

#Because we believe temperature and precipitation also matter, 
#construct the same variables for these.
#Note you could extend this to minimum, maximum, or other
#derivatives of weather/climate variables, but for illustration
#here we only use mean.
dta_Shp$pre_trend_temp_mean <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$post_trend_temp_01_10 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

dta_Shp$pre_trend_precip_mean <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")
dta_Shp$post_trend_precip_01_10 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

#Define a treatment variables.
#The script requires a binary 1 or 0 for units that were "Treated" 
#(i.e., had an intervention) and those that were not.
#Here, we define communities that were given legal status
#in the amazon before 2001 as "treated" communities,
#and communities after 2001 as "untreated".
dta_Shp@data["TrtBin"] <- 0
dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1

#Remove units that did not ever receive any treatment (within-sample test)
dta_Shp@data$NA_check <- 0
dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]
dta_Shp <- int_Shp

#This program currently uses a method called
#"Propensity Score Matching", which matches treatment
#and control communities based on their similarity
#in terms of a single "Propensity Score".
#The matches are weighted according to a calibrated
#distance-decay metric.

#The first step of this process is to calculate the PSM.  The 'psmmodel' predicts the chance
#of receiving treatment for each unit of analysis, and should include all variables you think
#might have impacted the treatment.
psmModel <-  "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp_mean + MeanP_1995 +
pre_trend_NDVI + Slope + Elevation +  MeanL_1995 + Riv_Dist + Road_dist +
pre_trend_precip_mean"

#This wil create a new column of data, "PSM_trtProb", held in psmRes$data.
#dta_Shp is the dataset (shapefile)
#method is the fitting method; logit is preferred in binary cases.
#psmModel is the model defined above
#drop="support" will remove observations that do not have a meaningful "comparison" case.
#visual=TRUE will output histograms showing before and after support drops.
psmRes <- SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=TRUE)

#Now, based on these treatment and control groups we want
#to identify "pairs" that are as similar as possible. However,
#we don't want to pair neighbors as it is possible 
#treatment effects may contain spillovers.
#To model this, we first identify the distance at which units
#are minimally similar to eachother ('spatial autocorrelation')
#This is generally the "lag distance" at which the Moran's I ceases to decrease,
#Or alternatively when it becomes non-significant.
#start is the closest distance, end is the farthest distance tested,
#and h is the number of "bins" of distance.
#In the plot produced in this example, the first bin ("1") is representative
#of how similar units between 10 and (590/20+10 = 39.5) kilometers away are to one another.
PSMdistDecay(psmRes$data,"PSM_trtProb",start=10,end=600,h=20)

#In this example, we can see that at 246 kilometers units become statistically
#uncorrelated with one another, and Moran's I is only a small positive in steps after this.
#Thus, we will use that threshold for a distance-decay penalty in our matching procedure.
#In the matching step, setting this penalizes units of observation closer than 246
#kilometers, with units 1km away receiving the largest penalty (thus being a highly unlikely match)

#Here, we create our treatment and control 
#First, we choose if we drop observations.  I.e., do we (a) drop observations that
#do not have a match (drop_unmatched), and (b) do we drop observations that do not
#have a match that is very similar?  To do (b), you can choose a drop method (right now,
#the best coded option is "SD", which is standard deviations of difference), and you set
#the drop_thresh - in this case, we have it equal to 0.25, so observations that have PSM
#scores more than 0.25 standard deviations away from one another will be dropped.
drop_set<- c(drop_unmatched=TRUE,drop_method="SD",drop_thresh=0.25)

#Second, we select the approach and constraints to our matches.
#The "FastNN" (though not always optimal) option for method is a fast approach to finding the nearest neighbor.
#Constraints can be use to geographically constrain matches - in this case we have a distance constrain
#that penalizes close pairs.
#psm_eq is the equation used in the first-stage PSM 
#ids are a unique ID field, and must be specified.
#drop_opts are the settings set above (line 126)
#visual enables visual outputs of the balance across control and treatment groups.
#TrtbinColName is the column that represents the binary (0/1) treatment variable.
psm_Pairs <- SAT(dta = psmRes$data, mtd = "fastNN",constraints=c(distance=246),psm_eq = psmModel, ids = "id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")

#In the histograms, you will see a clear alignment between treated and untreated in the PSM_trtProb variable.
#Generally, as you decrease the drop_thresh (i.e., make it more stringent), you will see improvement in
#"balance" across all variables (similar distributions), but also see a decreased sample size.
#In this example, the inital dataset was 106 units of observation.  This was cut down to 93 when
#observations were dropped on line 96 to facilitate common support.  Finally, it was cut down to 50
#(25 in each group) when pairs were matched on line 136, dropping pairs according to the settings
#on line 125.

#Now that we have our paired dataset, we can proceed as usual with our modeling strategy.
#Here, for illustration we simply conduct a linear fit.

analyticModel <-  "NDVI_trend_01_10 ~ TrtBin + terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp_mean + MeanP_1995 +
pre_trend_NDVI + Slope + Elevation +  MeanL_1995 + Riv_Dist + Road_dist +
pre_trend_precip_mean + post_trend_precip_01_10 + post_trend_temp_01_10"

#Here, you can either run a model manually, or with a wrapper command (Stage2PSM) in the package.
#The wrapper will automatically produce graphics and standardized (z-score) model output
#in the linear model case, but is limited to only two types of regression - linear models
#and 2-way clustering panel models.  

#Approach 1:
summary(lm(analyticModel, psm_Pairs))

#Approach 2:
Stage2PSM(analyticModel,psm_Pairs,type="lm",table_out=TRUE)

#In both of these models, the TrtBin variable indicates the significance (or, in this case, lack of)
#the treatment variable in driving the outcome variable.  
