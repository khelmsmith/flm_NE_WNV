#' @describeIn predict Generate leave-one-year-out cross-validated predictions from in-sample data
cross_validate = function(fittedModel, allLagsT, fillzeros, 
                          allunits, nsim){
  one_year <- function(t, fittedModel, 
           allLagsT, 
           fillzeros, allunits, nsim){
    training_data <- allLagsT[allLagsT$year != t,]
    test_data <- allLagsT[allLagsT$year == t,]
    form <- formula(fittedModel)
    fit <- gam(form, data=training_data, family=nb())
    return(predict_switch(fit, training_data, test_data, fillzeros, allunits, nsim))
  }
  years <- unique(allLagsT$year)
  cv_preds <- map(years, one_year, fittedModel = fittedModel,
                  allLagsT = allLagsT, fillzeros = fillzeros,
                  allunits = allunits, nsim = nsim)
  return(bind_rows(cv_preds))
}