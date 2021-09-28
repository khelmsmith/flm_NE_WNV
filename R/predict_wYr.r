#' Predict data, using imputation to get "new years"
#' 
#' @param fittedModel Model to predict from
#' @param allLagsT The assembled data set to use for training
#' @param allLagsO The assembled data set to use for out-of-sample prediction
#' @export
predict_wYr = function(fittedModel, allLagsT, allLagsO){
  
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
  allLagsO <- allLagsO[,c("County", "year", "cases", "fit", "se")]
  
  allLagsO <- dplyr::mutate(allLagsO, predcases = exp(fit))

  return(allLagsO)
}
