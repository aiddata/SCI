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

#Historic GIMMS NDVI
GIMMS_hist <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/historic_ndvi/historic_ndvi_extract_year_max.csv"
GIMMS_hist <- read.csv(GIMMS_hist)
#Rename columns...
for (i in 2:length(GIMMS_hist))
{
  colnames(GIMMS_hist)[i] <- sub("X","NDVI",colnames(GIMMS_hist)[i])
}
#Merge
kfw.SPDF <- merge(kfw.SPDF, GIMMS_hist, by.x="id", by.y="id")

#Contemporary GIMMS NDVI
GIMMS_cont <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/ndvi/ndvi_extract_year_max.csv"
GIMMS_cont <- read.csv(GIMMS_cont)
#Rename columns...
for (i in 2:length(GIMMS_cont))
{
  colnames(GIMMS_cont)[i] <- sub("X","NDVI",colnames(GIMMS_cont)[i])
}
#Merge
kfw.SPDF <- merge(kfw.SPDF, GIMMS_cont, by.x="id", by.y="id")



#=====================================================
#Exploratory / Visuals

#You can map any set of data using this command, but it is slow to render.
#ViewShp(kfw.SPDF,"Pop_1990","Brazil",4)

#Take a look at the NDVI records

NDVI_sub = kfw.SPDF@data[c(1,8:40)]
names(NDVI_sub) <- sub("NDVI","",names(NDVI_sub))
NDVI_long <- melt(NDVI_sub, id.vars=c("id"))

ggplot() + geom_point(data=NDVI_long, aes(x=value, y=variable))

#=============================================================
#Re-run the loads to correct the NDVI offset issue
#File for the KFW analysis
#This is ridiculously lazy and should be fixed.
shpfile = "Input_Data/KFW/Matched_Indigenous_Lands_id.shp"
src_Shp = readShapePoly(shpfile)

#Clean the source Shapefile to remove extra columns of data.
cln_Shp <- src_Shp[,c("terrai_nom","terrai_are","reu_id","id")]

#Population -------------------------------------------
GPW_pop <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/gpw/gpw_extract_merge.csv"
GPW_pop <- read.csv(GPW_pop)
#Rename the columns for easier interpretation later..
colnames(GPW_pop)[2] <- "Pop_1990"
colnames(GPW_pop)[3] <- "Pop_1995"
colnames(GPW_pop)[4] <- "Pop_2000"
#Merge it in
kfw.SPDF <- merge(cln_Shp, GPW_pop, by.x="id", by.y="id")

#Historic GIMMS NDVI
GIMMS_hist <- NA
GIMMS_hist <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/historic_ndvi/historic_ndvi_SIMULATED_yearly.csv"
GIMMS_hist <- read.csv(GIMMS_hist)
#Rename columns...
for (i in 2:length(GIMMS_hist))
{
  colnames(GIMMS_hist)[i] <- sub("_SIM_X","",colnames(GIMMS_hist)[i])
}
#Drop extra ID column
GIMMS_hist <- GIMMS_hist[-c(1)]

#Merge
kfw.SPDF <- merge(kfw.SPDF, GIMMS_hist, by.x="id", by.y="id")

#Contemporary GIMMS NDVI
GIMMS_cont <- "/mnt/sciclone-aiddata/REU/projects/kfw/extracts/ndvi/ndvi_extract_year_max.csv"
GIMMS_cont <- read.csv(GIMMS_cont)
#Rename columns...
for (i in 2:length(GIMMS_cont))
{
  colnames(GIMMS_cont)[i] <- sub("X","NDVI",colnames(GIMMS_cont)[i])
}
#Merge
kfw.SPDF <- merge(kfw.SPDF, GIMMS_cont, by.x="id", by.y="id")

NDVI_sub = kfw.SPDF@data[c(1,8:40)]
names(NDVI_sub) <- sub("NDVI","",names(NDVI_sub))
NDVI_long <- melt(NDVI_sub, id.vars=c("id"))

ggplot() + geom_point(data=NDVI_long, aes(x=value, y=variable))


