# forked from SPDEP (sp.correlogram)
PSM_correlogram <- function (dta, var, order = 1, style = "W", randomisation = TRUE, zero.policy = NULL, spChk = NULL, start, end) {

    stopifnot(is.vector(var))

    if (any(is.na(var))) 
        stop("no NAs permitted in variable")

    if (is.null(zero.policy)) 
        zero.policy <- get("zeroPolicy", envir = .spdepOptions)

    stopifnot(is.logical(zero.policy))

    if (is.null(spChk)) 
        spChk <- get.spChkOption()

    if (order < 1) 
        stop("order less than 1")
    
    #nblags <- nblag(neighbours, maxlag = order)
    #r.nb <- dnearneigh(as.matrix(coordinates(dta_prj)),d1=c1,d2=c2)

    end = end * 1000
    start = start * 1000
    rng_increment = (end-start) / order
    nblags <- vector(mode = "list", length = order)
    binname <- list()

    cur_step = start + rng_increment
    cur_start = start
    nblags[[1]] <- dnearneigh(dta, d1=cur_start, d2=cur_step)
    binname <- rbind(binname, (cur_start/1000))

    for (L in 2:order) {
        cur_start = cur_step
        cur_step = cur_step + rng_increment
        nblags[[L]] <- dnearneigh(dta, d1=cur_start, d2=cur_step)
        binname <- rbind(binname, (cur_start/1000))
    }

    cardnos <- vector(mode = "list", length = order)
    for (i in 1:order) {
        cardnos[[i]] <- table(card(nblags[[i]]))
    }

    nobs <- sapply(cardnos, function(x) sum(x[names(x) > "0"]))

    #if (any(nobs < 3)) 
    #  stop("sp.correlogram: too few included observations in higher lags:\n\treduce order.")
        res <- matrix(NA, nrow = 0, ncol = 3)
        cnt = 0
        for (i in 1:order) {
            if (nobs[[i]] == 0) {
                cardnos <- cardnos[-i]
                cnt = cnt + 1
                warn_str = paste("Bin h=",i,"was empty, and is excluded from the plot.")
                warning(warn_str)
            } else {
                listw <- nb2listw(nblags[[i]], style = style, zero.policy = zero.policy)
                res <- rbind(res, moran.test(var, listw, randomisation = randomisation, zero.policy = zero.policy)$estimate)
                cur_rw = i - cnt
                rownames(res)[cur_rw] <- binname[[i]]
            }
        }
        order = order - cnt
        #rownames(res) <- 1:order

    cardnos <- vector(mode = "list", length = order)
    for (i in 1:order) {
        cardnos[[i]] <- table(card(nblags[[i]])) 
    }

    obj <- list(res = res, method = "I", cardnos = cardnos, var = deparse(substitute(var)))
    class(obj) <- "spcor"
    print(obj)
    obj
}