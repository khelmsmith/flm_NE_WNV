
#' Fits the models to training data and generates the predictions
#'
#' @param allmods List of models
#' @param allLagsT Training data
#' @param allLagsO Out-of-sample data
#' @param fillzeros Logical. If TRUE fill in with zeros counties that have no cases ever.
#' @param allunits Character. Vector of county names in complete data set. Used 
#'   when fillzeros == TRUE
#' @param nsims Integer. Number of samples to draw from posterior distribution. Defaults to zero, which has the expected value of cases in predcases.
#'
#' @return Returns predictions for the out-of-sample year, a list of fitted models, AIC scores, and the model with the lowest AIC score
#' @export
#'
models_lags = function(allmods, allLagsT, allLagsO, fillzeros, allunits, nsims){
  allfits <- map(allmods, ~gam(as.formula(.x), data=allLagsT, family=nb()))
  # only predict from AIC best model
  AICfits <- map_dbl(allfits, MuMIn::AICc)
  best <- which.min(AICfits)
  form <- Reduce(paste, deparse(allfits[[best]]$formula[3]))
  
  if (grepl("year", form) == TRUE ){ 
    preds <- predict_wYr(allfits[[best]], allLagsT, allLagsO, fillzeros, allunits, nsims)
  } else if (grepl("year", form) == FALSE ) {
    preds <- predict_noYr(allfits[[best]], allLagsT, allLagsO, fillzeros, allunits, nsims)
  }

  return(list(predictions= preds,
              other = list(fittedModels=allfits,
                           AICfits = AICfits,
                           best = best)))
}



