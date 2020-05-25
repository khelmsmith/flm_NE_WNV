
#' fits the models to training data and generates the predictions.
#'
#' @param allmods 
#' @param allLagsT 
#' @param allLagsO 
#' @param results.path 
#'
#' @return
#' @export
#'
#' @examples
models_lags = function(allmods, allLagsT, allLagsO, results.path){
  allfits <- map(allmods, ~gam(as.formula(.x), data=allLagsT, family=nb()))
  # only predict from AIC best model
  AICfits <- map_dbl(allfits, MuMIn::AICc)
  best <- which.min(AICfits)
  form <- Reduce(paste, deparse(allfits[[best]]$formula[3]))
  newthing <- length(allfits) + 1
  allfits[[newthing]] <- ifelse (grepl("year", form), 1, 0)
  if (allfits[[newthing]] == 1){
    preds <- predict_wYr(allfits[[best]], allLagsT, allLagsO)
  } else if (allfits[[newthing]] == 0) {
    preds <- predict_noYr(allfits[[best]], allLagsT, allLagsO)
  }
 # preds <- predict_wYr(allfits[[best]], allLagsT, allLagsO)
  return(list(predictions= preds,
              other = list(fittedModels=allfits,
                           AICfits = AICfits,
                           best = best)))
}



