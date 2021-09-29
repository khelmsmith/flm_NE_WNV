
#' @describeIn predict Predict without year in model 
predict_noYr = function(fittedModel, allLagsT, allLagsO, fillzeros, allunits){

  # plug in model formula here
  modform <- formula(fittedModel)
  
  # predict for years included in training data
  
  # allYears <- unique(allLagsT$year)
  # listofallyears <- vector("list", length = length(allYears))
  # names(listofallyears) <- allYears
  # 
  # for (i in seq_along(allYears)) {
  #   yr.i <- allYears[i]
  #   indepData.i <- allLagsT[allLagsT$year != yr.i,]
  #   M.i <- gam(modform, data=indepData.i, family=nb())
  #   yrdata.i <- allLagsT[allLagsT$year == yr.i,]
  #   predyr.i <- predict(M.i, newdata=yrdata.i, type = "link", se=TRUE)
  #   # yrdata.i$testcol <- NULL
  #   yrdata.i$fit <- predyr.i[[1]]
  #   yrdata.i$se <- predyr.i[[2]]
  #   yrdata.i <- yrdata.i[,c("County", "year", "cases", "Lcases", "fit", "se")]
  #   listofallyears[[i]] <- yrdata.i
  # }
  # 
  # allIndepDataT <- do.call(rbind, listofallyears)
  
  # predict for out-of-sample data
  
  predsO <- predict(fittedModel, newdata=allLagsO, type = "link", se=TRUE)
  allLagsO$fit <- predsO[[1]]
  allLagsO$se <- predsO[[2]]
  allLagsO <- allLagsO[,c("County", "year", "cases", "Lcases", "fit", "se")]
  
  allLagsO <- dplyr::mutate(allLagsO, predcases = exp(fit))
  
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
                                 predcases = 0)
      allLagsO <- dplyr::bind_rows(allLagsO, missingunits)                               
    } else {
      message("No missing units found")
    }
  }
  
  return(allLagsO)
}
