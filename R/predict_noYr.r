
#' @describeIn predict Predict without year in model 
predict_noYr = function(fittedModel, allLagsT, allLagsO, unpredictedO, fillzeros, allunits, nsim){
  
  predsO <- predict(fittedModel, newdata=allLagsO, type = "link", se=TRUE)
  allLagsO$fit <- predsO[[1]]
  allLagsO$se <- predsO[[2]]
  
  if (nsim > 0){
    allLagsO$predcases <- simulate(fittedModel, newdata = allLagsO, nsim = nsim)
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
    if(sum(missingunits) != nrow(unpredictedO)) stop("bad missing units count in predict_noYr()")
    if (sum(missingunits) > 0){
      unpredictedO$predcases = ifelse(nsim > 0, 
                                      matrix(0, nrow = length(missingunits),
                                             ncol = nsim),
                                      0)
      allLagsO <- dplyr::bind_rows(allLagsO, unpredictedO)                               
    } else {
      message("No missing units found")
    }
  }
  
  return(allLagsO)
}
