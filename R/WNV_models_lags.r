
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
