#' Predict data without using year
#' 
#' @param allLagsT The assembled data set to use for training
#' @param allLagsO The assembled data set to use for out-of-sample prediction
#' 
predict_noYr = function(fittedModel, allLagsT, allLagsO){

  # plug in model formula here
  modform <- formula(fittedModel)
  
  # predict for years included in training data
  
  allYears <- unique(allLagsT$year)
  listofallyears <- vector("list", length = length(allYears))
  names(listofallyears) <- allYears
  
  for (i in seq_along(allYears)) {
    yr.i <- allYears[i]
    indepData.i <- allLagsT[allLagsT$year != yr.i,]
    M.i <- gam(modform, data=indepData.i, family=nb())
    yrdata.i <- allLagsT[allLagsT$year == yr.i,]
    predyr.i <- predict(M.i, newdata=yrdata.i, type = "link", se=TRUE)
    # yrdata.i$testcol <- NULL
    yrdata.i$fit <- predyr.i[[1]]
    yrdata.i$se <- predyr.i[[2]]
    yrdata.i <- yrdata.i[,c("County", "year", "cases", "Lcases", "fit", "se")]
    listofallyears[[i]] <- yrdata.i
  }
  
  allIndepDataT <- do.call(rbind, listofallyears)
  
  # predict for out-of-sample data
  
  predsO <- predict(fittedModel, newdata=allLagsO, type = "link", se=TRUE)
  allLagsO$fit <- predsO[[1]]
  allLagsO$se <- predsO[[2]]
  allLagsO <- allLagsO[,c("County", "year", "cases", "Lcases", "fit", "se")]
  
  allIndepData <- rbind(allIndepDataT, allLagsO)
  
  # exponentiate fit to get predictions from model with log link
  
  allIndepData <- dplyr::mutate(allIndepData, pred = exp(fit))

  return(allLagsO)
}
