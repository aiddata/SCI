#Initialization Script
#To test sub-modules of C-SAT

#Load required libraries from our dependencies script.
#source('Dependencies/dep.R', chdir=T)

#Load user input settings
#source('user_inputs.R', chdir=T)

#Load descriptive tools
#source('Tools/descriptives.R', chdir=T)

#Custom Functions
#source('functions.R', chdir=T)

#Later tool loads will go here - closeness, matching, balancing, modeling.

#--------------------------------
#Tool runs
runApp(".")

#descTrtCont(shp=user_shape, flds=(c("Accepted_Y","NDVI_","UF","pop_FUNAI2"),c("con","time","factor","con")), method=c("thresh","manual",2002,"<"))

#psmAsumTests()
