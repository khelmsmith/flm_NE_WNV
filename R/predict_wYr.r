#' Predict data
#' 
#' @param fittedModel Model to predict from
#' @param allLagsT The assembled data set to use for training
#' @param allLagsO The assembled data set to use for out-of-sample prediction
#' @param fillzeros Logical. If TRUE fill in with zeros counties that have no cases ever.
#' @param allunits Character. Vector of county names in complete data set. Used 
#'   when fillzeros == TRUE
#' @param nsim Integer. Number of samples to draw from posterior distribution. Defaults to zero, which has the expected value of cases in predcases.
#' @name predict
#' @export
NULL

#' @describeIn predict Predict from models including year
predict_wYr = function(fittedModel, allLagsT, allLagsO, fillzeros, allunits, nsim){
  
  # Plug in the model formula here
  modform <- formula(fittedModel)
  
  modsum <- summary(fittedModel)
  
  # extracting the fitted yr coefficients
  ptab <- as.data.frame(modsum$p.table)
  years<-  grepl("year", row.names(ptab))
  ptab <- ptab[years, 1, drop=FALSE]
  colnames(ptab) <- "yrcoef"
  ptab[nrow(ptab)+1,"yrcoef"] <- -sum(ptab$yrcoef)
  ptab$year <- factor(levels(allLagsT$year))
  contrasts(ptab$year) <- contrasts(allLagsT$year)
  
  allLagsT <- merge(allLagsT, ptab) 
  
  # predict the year coefficient with training data 
  
  # change tmean and SP(E)I variable lag lengths here to match model formula
  # yrcoefmodform <- update(modform, yrcoef~.-year-offset(log(pop100K)))
  # yrcoefmod <- gam(yrcoefmodform, data=allLagsT, family=gaussian())
  # do it the hard way, because we don't know what other terms are in the model
  # and update doesn't remove the offset.
  form <- Reduce(paste, deparse(fittedModel$formula[3]))
  form <- gsub('^.|.$', '', form)
  form <- trimws(form)  
  formterms <- unlist(strsplit(form, "+", fixed = TRUE))
  numterms <- length(formterms)
  termtab <- data.frame(matrix(unlist(formterms), nrow=numterms, byrow=TRUE),stringsAsFactors=FALSE)
  colnames(termtab) <- "terms"
  termtab <- termtab %>% mutate(keep = ifelse(grepl("s\\(", terms), "yes", "no"))
  termtab <- termtab[termtab$keep == "yes",]
  newterms <- toString(paste0(termtab$terms, sep = "+"))
  newterms <- gsub("\\+,","\\+", newterms)
  yrcoefmodform <- paste0("yrcoef ~ ",newterms ," County")
  yrcoefmodform <- as.formula(yrcoefmodform)
  
  yrcoefmod <- gam(yrcoefmodform, data=allLagsT, family=gaussian())
  
  # use the year coefficient to predict coyr for each county-year on training data
  new <- predict(yrcoefmod, newdata = allLagsT, type = "link", se = TRUE)
  allLagsT$coyr <- new$fit
  allLagsT$coyrse <- new$se.fit
  allLagsT$yrcoef <- NULL

  new <- predict(yrcoefmod, newdata = allLagsO, type = "link", se = TRUE)
  allLagsO$coyr <- new$fit
  allLagsO$coyrse <- new$se.fit
  allLagsO$yrcoef <- NULL
  
  #refit the model with training data
  predmod <- gam(update(modform, . ~ . - year + coyr), data=allLagsT, family=nb())
  # summary(mod2)

  # now predict using the new coyr variable instead of the year coefficient
  preds <- predict(predmod, newdata=allLagsO, type = "link", se=TRUE) 
  allLagsO$fit <- preds[[1]]
  allLagsO$se <- preds[[2]]
  
  if (nsim > 0){
    allLagsO$predcases <- simulate(predmod, newdata = allLagsO, nsim = nsim)
  } else {
    allLagsO$predcases <- exp(allLagsO$fit)
  }

  allLagsO <- allLagsO[,c("County", "year", "cases", "fit", "se", "predcases")]
  
  if (fillzeros){
    message("Filling in counties with no cases with zero predictions.")
    # allunits has all counties, including those with zeros.
    # extract counties in cases that are NOT in results$predictions
    # to identify counties to fill in
    missingunits <- !(allunits %in% unique(allLagsO$County))
    if (sum(missingunits) > 0){
      missingunits <- allunits[missingunits]
      missingunits <- data.frame(County = missingunits,
                                 year = allLagsO$year[1],
                                 cases = 0,
                                 fit = NA_real_,
                                 se = NA_real_,
                                 predcases = ifelse(nsim > 0, matrix(0, nrow = length(missingunits),
                                                                     ncol = nsim),
                                                    0))
      allLagsO <- dplyr::bind_rows(allLagsO, missingunits)                               
    } else {
      message("No missing units found")
    }
  }
  
  return(allLagsO)
}
