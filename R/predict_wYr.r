#' Predict data without using year
#' 
#' @param allLagsT The assembled data set to use for training
#' @param allLagsO The assembled data set to use for out-of-sample prediction
#' 
predict_wYr = function(allLagsT, allLagsO){
  
  # Get min and max year from training data
  yrmin <- min(allLagsT$year)
  yrmax <- max(allLagsT$year)
  
  # Plug in the model formula here
  modform <- cases ~ s(lags_tmean30, by = tmean30) + s(lags_spi24, by = spi24) + CI + County + year + offset(log(pop100K))
  
  mod <- gam(modform, data=allLagsT, family=nb())
  
  modsum <- summary(mod)
  
  ptab <- as.data.frame(modsum$p.table)
  
  years<-  grepl("year", row.names(ptab))
  ptab <- ptab[years, 1, drop=FALSE]
  colnames(ptab) <- "yrcoef"
  ptab[nrow(ptab)+1,"yrcoef"] <- -sum(ptab$yrcoef)
  
  ptab$year <- as.factor(c(yrmin:yrmax))
  
  allLagsT <- merge(allLagsT, ptab) 
  
  # predict the year coefficient with training data 
  
  # change tmean and SP(E)I variable lag lengths here to match model formula
  
  M1 <- gam(yrcoef ~ s(lags_tmean30, by = tmean30) + s(lags_spi24, by = spi24) + County, data=allLagsT, family=gaussian())
  
  # use the year coefficient to predict coyr for each county-year on training data
  new <- predict(M1, newdata = allLagsT, type = "link", se = TRUE)
  allLagsT$coyr <- new$fit
  allLagsT$coyrse <- new$se.fit
  allLagsT$yrcoef <- NULL
  
  #refit the model with training data
  mod2 <- gam(update(modform, . ~ . - year + coyr), data=allLagsT, family=nb())
  # summary(mod2)

  # now predict using the new coyr variable instead of the year coefficient
  
  # first for training data
  
  allLagsT$year <- as.factor(allLagsT$year)
  allYears <- levels(allLagsT$year)
  listofallyears <- vector("list", length = length(allYears))
  names(listofallyears) <- allYears
  cs <- length(allYears)
  
  for (i in seq_along(allYears)) {
    yr.i <- allYears[i]
    indepData.i <- allLagsT[allLagsT$year != yr.i,]
    indepData.i$year <- droplevels(indepData.i$year)
    contrasts(indepData.i$year) = contr.sum(cs-1)
   mod2 <- gam(update(modform, . ~ . - year + coyr), data = indepData.i, family=nb())
  
  yrdata.i <- allLagsT[allLagsT$year == yr.i,]
  predyr.i <- predict(mod2, newdata=yrdata.i, type = "link", se=TRUE) #mod2 is already created
    yrdata.i$fit <- predyr.i[[1]]
    yrdata.i$se <- predyr.i[[2]]
    yrdata.i <- yrdata.i[,c("County", "year", "cases", "Lcases", "fit", "se")]
    row.names(yrdata.i) <- NULL
    listofallyears[[i]] <- yrdata.i
  }
  allIndepDataT <- do.call(rbind, listofallyears)
  
  # then for out-of-sample data
  
  new <- predict(M1, newdata = allLagsO, type = "link", se = TRUE)
  allLagsO$coyr <- new$fit
  allLagsO$coyrse <- new$se.fit
  
  preds <- predict(mod2, newdata=allLagsO, type = "link", se=TRUE) 
    allLagsO$fit <- preds[[1]]
    allLagsO$se <- preds[[2]]
    allLagsO <- allLagsO[,c("County", "year", "cases", "Lcases", "fit", "se")]
  
  # combine them   
  
  allIndepDataT$year <- as.character(allIndepDataT$year)
  allIndepData <- rbind(allIndepDataT, allLagsO)
  
  # exponentiate fit to get predictions from log-linked model
  
  allIndepData <- dplyr::mutate(allIndepData, pred = exp(fit))

  return(list(allIndepData, mod))
}
