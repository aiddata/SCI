#Wrapper for simulation tests of SpatialCausal package.
 #Generating Random Spatial Fields

library(devtools)
devtools::install_github("itpir/SAT")
library(SAT)

SAT::loadLibs()

#Number of Iterations
its = 1

#How big the field will be
x <- seq(1, 10, 1)

#Equations to fit
PSM_eq = "TrtBin ~ ControlA"
Final_eq = "y ~ TrtBin + ControlA"

#-----------------------------------------
iteration = 1
beta_df <- data.frame(matrix(ncol=7, nrow=0))
colnames(beta_df)[1] <- "YMorans"
colnames(beta_df)[2] <- "TMorans"
colnames(beta_df)[3] <- "XMorans"
colnames(beta_df)[4] <- "EAMorans"
colnames(beta_df)[5] <- "EBMorans"
colnames(beta_df)[6] <- "Model_Type"
colnames(beta_df)[7] <- "BdifBhat"

while (iteration <= its)
{
  #rho_opt=c("ControlA,RandomFieldA,ControlB,RandomFieldB,TrtBin")
  f.SPDF <- SAT::SpatialCausalSim_DGP(fld_size = x,SpatialCov_opt=c("ControlA,RandomFieldA,ControlB,RandomFieldB,TrtBin"),rho_mult=5)
  
  f.NB = poly2nb(f.SPDF)
  f.W = nb2listw(f.NB, style='W')
  
  psm_Res <- SAT::SpatialCausalPSM(dta = f.SPDF, mtd = "logit", PSM_eq, drop="overlap", visual="TRUE")
  psm_Pairs <- SAT::SpatialCausalDist(dta = psm_Res, mtd = "fastNN", vars = PSM_eq, ids = "simIDs", drop_unmatched = TRUE, drop_method = "SD", drop_thresh=.5, visual="FALSE")
  
  #Test various models and record resulst...
  #Linear Model - Simplest OLS
  sim_lm <- lm(Final_eq, data=psm_Pairs)
  bdif = sim_lm$coefficients["TrtBin"][[1]] - 1.0
  df_simlm <- c(moran.test(f.SPDF@data$y, f.W)$estimate[[1]],
                moran.test(f.SPDF@data$TrtBin, f.W)$estimate[[1]],
                moran.test(f.SPDF@data$ControlA, f.W)$estimate[[1]],
                moran.test(f.SPDF@data$RandomFieldA, f.W)$estimate[[1]],
                moran.test(f.SPDF@data$RandomFieldB, f.W)$estimate[[1]],
                "LM",
                bdif
                )
  row_betadf <- length(beta_df[[1]])
  beta_df[(row_betadf+1),] <- df_simlm
  
  
  
  print(iteration)
  iteration = iteration + 1
  
}

plot(beta_df$TMorans, beta_df$BdifBhat)
plot(beta_df$XMorans, beta_df$BdifBhat)
plot(beta_df$YMorans, beta_df$BdifBhat)

beta_df$AvgMorans = (as.numeric(beta_df$TMorans) + as.numeric(beta_df$XMorans) + as.numeric(beta_df$YMorans)) / 3.0
plot(beta_df$AvgMorans, beta_df$BdifBhat)
