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
shpfile = "example.shp"
dta_Shp = readShapePoly(shpfile)



