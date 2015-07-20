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

dta_Shp$pre_trend_NDVI_mean <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")




