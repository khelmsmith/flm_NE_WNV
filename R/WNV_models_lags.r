#' Fits the models to training data and generates the predictions
#'
#' @param allmods List of models
#' @param allLagsT Training data
#' @param allLagsO Out-of-sample data
#' @param fillzeros Logical. If TRUE fill in with zeros counties that have no cases ever.
#' @param allunits Character. Vector of county names in complete data set. Used 
#'   when fillzeros == TRUE
#' @param nsim Integer. Number of samples to draw from posterior distribution. Defaults to zero, which has the expected value of cases in predcases.
#' @param predict_from Character. predict from AIC best model only, or from all models?
#'
#' @return Returns predictions for the out-of-sample year, a list of fitted models, AIC scores, and the model with the lowest AIC score. The 
#' predictions object could be a vector (nsim = 0 and predict_from == "best"),
#' a matrix (nsim > 0 and predict_from == "best"),
#' or a list of vectors or matrices (predict_from == "all")
#' @export
#'
models_lags = function(allmods, allLagsT, allLagsO, fillzeros, allunits, nsim, predict_from = c("best","all")){

  allfits <- map(allmods, ~gam(as.formula(.x), data=allLagsT, family=nb()))
  AICfits <- map_dbl(allfits, MuMIn::AICc)
  best <- which.min(AICfits)

  predict_from <- match.arg(predict_from)
  if (predict_from == "best"){
    # only predict from AIC best model
    preds <- predict_switch(allfits[[best]], allLagsT, allLagsO, fillzeros, allunits, nsim)
  } else if (predict_from == "all") {
    # TODO: this is incorrect -- need CRPS from all models on either in sample or cross-validated out of sample data
    preds <- map(allfits, cross_validate, allLagsT, fillzeros, allunits, nsim)
  } else {
    stop("Unknown choice in predict_from argument to model_lags")
  }

  return(list(predictions = preds,
              other = list(fittedModels=allfits,
                           AICfits = AICfits,
                           best = best)))
}

#' Parse the model formula and call the appropriate predict function
#'
#' @param fittedModel Model to predict from
#' @param allLagsT The assembled data set to use for training
#' @param allLagsO The assembled data set to use for out-of-sample prediction
#' @param fillzeros Logical. If TRUE fill in with zeros counties that have no cases ever.
#' @param allunits Character. Vector of county names in complete data set. Used 
#'   when fillzeros == TRUE
#' @param nsim Integer. Number of samples to draw from posterior distribution. Defaults to zero, which has the expected value of cases in predcases.
#'
#' @return
#'
predict_switch <- function(fittedmodel, allLagsT, allLagsO, fillzeros, allunits, nsim){
  form <- Reduce(paste, deparse(fittedmodel$formula[3]))
  
  if (grepl("year", form) == TRUE ){ 
    preds <- predict_wYr(fittedmodel, allLagsT, allLagsO, fillzeros, allunits, nsim)
  } else if (grepl("year", form) == FALSE ) {
    preds <- predict_noYr(fittedmodel, allLagsT, allLagsO, fillzeros, allunits, nsim)
  }
  return(preds)
}

