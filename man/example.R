#Example script for the use of the
#Spatial Causal Inference package in R

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

#The first step of this process is to calculate the PSM:
psmModel <-  "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp_mean + MeanP_1995 +
pre_trend_NDVI + Slope + Elevation +  MeanL_1995 + Riv_Dist + Road_dist +
pre_trend_precip_mean"

psmRes <- SAT::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=TRUE)
