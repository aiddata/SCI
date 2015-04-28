#Library loading script in case dependencies are not loading correctly.
loadLibs <- function (x=1)
  {
  require(sp)
  #require(GISTools) # sudo apt-get install libgeos-dev
  require(maptools)
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
  library(texreg)
  require(proj4)
  }