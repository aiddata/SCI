#Analytics and Models
#Note most users will likely want to run their own models.
#These functions are to make common modeling strategies easier to specify for users
#That do not write their own models.

Stage2PSM <- function (model, dta, type, table_out = NULL, opts = NULL) {
  
    ret_var <- list()
  
    if (type == "lm") {
        m_fit <- lm(model, dta)
        print("==========================")
        print("UNSTANDARDIZED MODEL RESULTS")
        print("==========================")
        #mTab <- stargazer(m_fit,type="html",title="Unstandardized Model Results")
        print(summary(m_fit))
        ret_var$unstandardized <- lm(model, dta)
        texreg::plotreg(m_fit, omit.coef="(match)|(Intercept)", custom.model.names="Unstandardized Model", custom.note=model)
        
        if (!is.null(table_out)) {
            dta_tmp <- dta
      
            if (class(dta) == "data.frame") {
                d_index <- sapply(dta_tmp, is.numeric)
                dta_tmp[d_index] <- lapply(dta_tmp[d_index],scale)
            } else {
                d_index <- sapply(dta_tmp@data, is.numeric)
                dta_tmp@data[d_index] <- lapply(dta_tmp@data[d_index],scale)
            }

            dta_fit_std <- lm(model,dta_tmp)
            ret_var$standardized <- lm(model,dta_tmp)
            print("==========================")
            print("STANDARDIZED MODEL RESULTS")
            print("==========================")
            print(summary(dta_fit_std))
            texreg::plotreg(dta_fit_std,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model", custom.note=model)
          
        }
    
    }
  
    if (type == "cmreg") {

        # make sure cluster options provided are valid
        if (length(opts) == 0 || opts == NULL) {
            print("Must have at least 1 clustering option")
            return("Invalid opts given.")

        } else if (length(opts) > 2) {
            print("Cannot have more than 2 clustering options")
            return("Invalid opts given.")
        }


        print("c1a")
        m_fit <- lm(model, dta)

        print("c1b")
        ret_var$unstandardized <- m_fit

        print("c1c")
        #mTab <- stargazer(m_fit,type="html",title="Unstandardized Model Results")
        #print.htmlTable(mTab)
        print(summary(m_fit))
        texreg::plotreg(m_fit,omit.coef="(match)|(Intercept)|(factor)",custom.model.names="Unstandardized Model",custom.note=model)

        print("c2")

        if (!is.null(table_out)) {
            dta_tmp <- dta
          
            if( class(dta) == "data.frame") {
                d_index <- sapply(dta_tmp, is.numeric)
                dta_tmp[d_index] <- lapply(dta_tmp[d_index],scale)
            } else {
                d_index <- sapply(dta_tmp@data, is.numeric)
                dta_tmp@data[d_index] <- lapply(dta_tmp@data[d_index],scale)
            }
            dta_fit_std <- lm(model,dta_tmp)
            ret_var$standardized <- dta_fit_std
            print(summary(dta_fit_std))
            texreg::plotreg(dta_fit_std,omit.coef="(match)|(Intercept)|(factor)",custom.model.names="Standardized Model", custom.note=model)
          
        }

        print("c3")
    
        print(opts)

        # exec = paste("cluster.vcov(m_fit,cbind(dta$",opts[1],",dta$",opts[2],"))",sep="")
        # m_fit[["var"]] <- eval(parse(text=exec))

        if (length(opts) == 1) {
            m_fit$var <- cluster.vcov(m_fit,dta[opts[1]])

        } else if (length(opts) == 2) {
            m_fit$var <- cluster.vcov(m_fit,cbind(dta[opts[1]], dta[opts[2]]))

        }

        CMREG <- coeftest(m_fit,m_fit$var)
        print("cmReg:")
        print(CMREG)
        ret_var$cmreg <- CMREG
        
    }
    return(ret_var)
}
