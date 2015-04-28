#forked from SPDEP (sp.correlogram)

PSM_correlogram <- function (neighbours, var, order = 1, style = "W", 
          randomisation = TRUE, zero.policy = NULL, spChk = NULL) 
{
  if (class(neighbours) != "nb") 
    stop("not a neighbours list")
  stopifnot(is.vector(var))
  if (any(is.na(var))) 
    stop("no NAs permitted in variable")
  if (is.null(zero.policy)) 
    zero.policy <- get("zeroPolicy", envir = .spdepOptions)
  stopifnot(is.logical(zero.policy))
  if (is.null(spChk)) 
    spChk <- get.spChkOption()
  if (spChk && !chkIDs(var, nb2listw(neighbours, zero.policy = zero.policy))) 
    stop("Check of data and weights ID integrity failed")
  if (order < 1) 
    stop("order less than 1")
  nblags <- nblag(neighbours, maxlag = order)
  cardnos <- vector(mode = "list", length = order)
  for (i in 1:order) cardnos[[i]] <- table(card(nblags[[i]]))
  nobs <- sapply(cardnos, function(x) sum(x[names(x) > "0"]))
  #if (any(nobs < 3)) 
  #  stop("sp.correlogram: too few included observations in higher lags:\n\treduce order.")
    res <- matrix(NA, nrow = order, ncol = 3)
    for (i in 1:order) {
      if(nobs[[i]] == 0)
      {
        res[i ,] <- c(-999,-999,-999)
      }
      else
      {
      listw <- nb2listw(nblags[[i]], style = style, zero.policy = zero.policy)
      res[i, ] <- moran.test(var, listw, randomisation = randomisation, 
                               zero.policy = zero.policy)$estimate
      }
    }
    rownames(res) <- 1:order

  for (k in 1:length(res[,1]))
  {
    if(res[[k]][1] == -999)
    {    
      res <- res[-k]
    }
  }
  
  obj <- list(res = res, method = "I", cardnos = cardnos, 
              var = deparse(substitute(var)))
  class(obj) <- "spcor"
  print(obj)
  obj
}