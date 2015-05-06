#Analytics and Models
#Note most users will likely want to run their own models.
#These functions are to make common modeling strategies easier to specify for users
#That do not write their own models.

Stage2PSM <- function(model, dta, type, table_out = NULL, opts = NULL)
{
  

  
  if(type == "lm")
  {
    m_fit <- lm(model,dta)
    mTab <- stargazer(m_fit,type="html",title="Unstandardized Model Results")
    print.htmlTable(mTab)
    texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Unstandardized Model",custom.note=model)
    
    if(!is.null(table_out))
    {
      dta_tmp <- dta
      
      if(class(dta) == "data.frame")
      {
        d_index <- sapply(dta_tmp, is.numeric)
        dta_tmp[d_index] <- lapply(dta_tmp[d_index],scale)
      } else {
      d_index <- sapply(dta_tmp@data, is.numeric)
      dta_tmp@data[d_index] <- lapply(dta_tmp@data[d_index],scale)
      }
      dta_fit_std <- lm(model,dta_tmp)
      texreg::plotreg(dta_fit_std,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model", custom.note=model)
      
    }
    
  }
  
  if(type == "cmreg")
  {
    m_fit <- lm(model,dta)
    mTab <- stargazer(m_fit,type="html",title="Unstandardized Model Results")
    print.htmlTable(mTab)
    texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)",custom.model.names="Unstandardized Model",custom.note=model)
    
    if(!is.null(table_out))
    {
      dta_tmp <- dta
      
      if(class(dta) == "data.frame")
      {
        d_index <- sapply(dta_tmp, is.numeric)
        dta_tmp[d_index] <- lapply(dta_tmp[d_index],scale)
      } else {
        d_index <- sapply(dta_tmp@data, is.numeric)
        dta_tmp@data[d_index] <- lapply(dta_tmp@data[d_index],scale)
      }
      dta_fit_std <- lm(model,dta_tmp)
      texreg::plotreg(dta_fit_std,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model", custom.note=model)
      
    }
    
    print(opts)
    exec = paste("cluster.vcov(m_fit,cbind(dta$",opts[1],",dta$",opts[2],"))",sep="")
    m_fit$var <- eval(parse(text=exec))
    CMREG <- coeftest(m_fit,m_fit$var)
    print(CMREG)
    
  }
}