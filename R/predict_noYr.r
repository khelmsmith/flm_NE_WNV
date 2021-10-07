
#' @describeIn predict Predict without year in model 
predict_noYr = function(fittedModel, allLagsT, allLagsO, fillzeros, allunits, nsim){

  predsO <- predict(fittedModel, newdata=allLagsO, type = "link", se=TRUE)
  allLagsO$fit <- predsO[[1]]
  allLagsO$se <- predsO[[2]]
  
  if (nsim > 0){
    allLagsO$predcases <- simulate(fittedModel, newdata = allLagsO, nsim = nsim)
  } else {
    allLagsO$predcases <- exp(allLagsO$fit)
  }

  allLagsO <- allLagsO[,c("County", "year", "cases", "Lcases", "fit", "se", "predcases")]
  
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
