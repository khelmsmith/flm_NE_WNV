
#' fits the models to training data and generates the predictions.
#'
#' @param models 
#' @param allLagsT 
#' @param allLagsO 
#' @param results.path 
#'
#' @return
#' @export
#'
#' @examples
models_lags = function(models, allLagsT, allLagsO, results.path){
  allfits <- map(models, ~gam(as.formula(.x), data=allLagsT, family=nb()))
  # only predict from AIC best model
  AICfits <- map_dbl(allfits, MuMIn::AICc)
  best <- which.min(AICfits)
  preds <- predict_wYr(allfits[[best]], allLagsT, allLagsO)
  return(list(predictions= preds,
              other = list(fittedModels=allfits,
                           AICfits = AICfits,
                           best = best)))
}
