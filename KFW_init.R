#Load required functions from SAT
source('Dependencies/dep.R', chdir=T)
source('Tools/descriptives.R', chdir=T)
source('Tools/SpatialCausalPSM.R', chdir=T)
source('functions.R', chdir=T)

#File for the KFW analysis
shpfile = "Input_Data/KFW/Matched_Indigenous_Lands_id.shp"
src_Shp = readShapePoly(shpfile)

#Clean the source Shapefile to remove extra columns of data.
cln_Shp <- src_Shp[,c("terrai_nom","terrai_are","reu_id","id")]

#Load in the data to join to the shapefile
#======================================================
#Population -------------------------------------------
GPW_pop <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/gpw/gpw_extract_merge.csv"
GPW_pop <- read.csv(GPW_pop)
#Rename the columns for easier interpretation later..
colnames(GPW_pop)[2] <- "Pop_1990"
colnames(GPW_pop)[3] <- "Pop_1995"
colnames(GPW_pop)[4] <- "Pop_2000"
#Merge it in
kfw.SPDF <- merge(cln_Shp, GPW_pop, by.x="id", by.y="id")
#You can map any set of data using this command, but it is slow to render.
#ViewShp(kfw.SPDF,"Pop_1990","Brazil",4)

