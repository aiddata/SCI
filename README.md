# SCI
Spatial Causal Inferential Package for R

Below is an exmaple for the use of this package - you can also find this
example, with much more extensive documentation,
in the /man directory of the package, along with an example.shp file.

Please note this package is in an early alpha release, and as such
has many instabilities, bugs and errors, and is limited in functionality.
If you encounter issues, do not hesitate to contact me (drunfola@aiddata.org).

#Package and data loading

library(devtools)

devtools::install_github("itpir/SCI@master")

library(SCI)

shpfile = file.path(getwd(),"man","data","example.shp")

dta_Shp = readShapePoly(shpfile)

#Variable construction examples
dta_Shp$pre_trend_NDVI <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

dta_Shp$NDVI_trend_01_10 <- timeRangeTrend(dta_Shp,"MeanL_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

dta_Shp$pre_trend_temp_mean <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

dta_Shp$post_trend_temp_01_10 <- timeRangeTrend(dta_Shp,"MeanT_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

dta_Shp$pre_trend_precip_mean <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"SP_ID")

dta_Shp$post_trend_precip_01_10 <- timeRangeTrend(dta_Shp,"MeanP_[0-9][0-9][0-9][0-9]",2001,2010,"SP_ID")

dta_Shp@data["TrtBin"] <- 0

dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1

dta_Shp@data$NA_check <- 0

dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1

int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]

dta_Shp <- int_Shp

#Modeling examples
psmModel <-  "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp_mean + MeanP_1995 +
pre_trend_NDVI + Slope + Elevation +  MeanL_1995 + Riv_Dist + Road_dist +
pre_trend_precip_mean"

psmRes <- SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=TRUE)

PSMdistDecay(psmRes$data,"PSM_trtProb",start=10,end=600,h=20)

drop_set<- c(drop_unmatched=TRUE,drop_method="SD",drop_thresh=0.25)

psm_Pairs <- SAT(dta = psmRes$data, mtd = "fastNN",constraints=c(distance=246),psm_eq = psmModel, ids = "id", \

drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")

analyticModel <-  "NDVI_trend_01_10 ~ TrtBin + terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp_mean + MeanP_1995 + pre_trend_NDVI + Slope + Elevation +  MeanL_1995 + Riv_Dist + Road_dist + pre_trend_precip_mean"

summary(lm(analyticModel, psm_Pairs))

Stage2PSM(analyticModel,psm_Pairs,type="lm",table_out=TRUE)


