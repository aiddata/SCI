#Library loading script, to keep it all in one place...
loadLibs <- function (x=1)
  {
  require(sp)
  #require(GISTools) # sudo apt-get install libgeos-dev
  require(maptools)
  #require(DT)
  require(ggplot2)
  require(FNN)
  require(psych)
  library(spdep) 
  library(RandomFields)
  library(ggmap) #sudo apt-get install r-cran-rjson
  library(reshape)
  library(grid)
  library(gridExtra)
  library(QuantPsyc)
  }