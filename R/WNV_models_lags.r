
models_lags = function(models, allLagsT, allLagsO, results.path){
  
  allfits <- map(models, ~gam(as.formula(.x), data=allLagsT, family=nb()))
  preds <- predict_wYr(allfits[[1]], allLagsT, allLagsO)
  return(preds)
}
