

#Function to compare any 2 models in terms of their estimation on treatment effect.
#In our case, these are generally pre-defined models (i.e., models from a DGP)
#This function returns a Moran's I measurement of spatial autocorrelation in the outcome, treatment, and average of all controls.
#It also returns the difference in beta coefficients on the treatment for each model.