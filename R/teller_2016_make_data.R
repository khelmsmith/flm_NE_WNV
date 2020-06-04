#' Function from Teller 2016 (doi 10.1111/2041-210X.12486) to create distributed lags of monthly data

aggLags<-function(datC, fun, meas, seg){
  pars <- as.list(match.call()[-1])
  measure <- datC[, meas]
  segment <- datC[, as.character(pars$seg)]
  agg <- with(datC, ftable(tapply(measure, list(datC$year, segment), fun)))
  agg <- as.data.frame(agg)
  names(agg) <- c("year", pars$seg, paste(pars$meas, pars$fun,"Now", sep=""))
  
  return(list(agg,paste(pars$fun)))
}

# function from Teller 2016 to create lagged variables
appLags<-function(agDatC, nUnits, name, fun){
  storeNames<-names(agDatC)
  for (i in 1:nUnits){
    agDatC <- as.data.frame(agDatC)
    waved <- agDatC[[3]]
    newWave <- c(rep(NA, times=i),waved) 
    newThing <- newWave[1:nrow(agDatC)]
    agDatC <- cbind(agDatC, newThing)
  }
  names(agDatC) <- c(storeNames,paste(name,fun,1:nUnits, sep=""))
  return(agDatC)
}

# now put it all together
#' Assemble data with lagged environmental predictors
#'
#' @param envData values of environmental data, in this case, by month and county
#' @param response the response variable, human cases of WNV
#' @param monthStart the month from which to lag the environmental data back
#' @param meas month
#' @param nUnits how many units (meas) to go back
#'
#' @return a matrix of lagged environmental variables
#' @export

makeDat = function(envData, response, monthStart, meas, nUnits){

  # aggregates data to longer segments (e.g. daily -> monthly)
  AggEnv <- aggLags(datC=envData, fun=sum, meas=meas, seg=month)
  subAgg <- AggEnv[[1]]
  subAgg$month<-as.numeric(as.character(subAgg$month))
  subAgg<-subAgg[with(subAgg,order(year,month)),]
  
  
  envLagged<-appLags(agDatC=subAgg, nUnits=nUnits, name=paste("month",as.character(meas), sep=""), fun=AggEnv[[2]])
  envLagged <- na.omit(envLagged)
  
  #monthStart=monthStart
  subsWT<-subset(envLagged,envLagged$month==monthStart) #choose start month
  subsWT<-subsWT[,-which(names(subsWT)=="month")] #Get rid of "month" column
  subsWT$year<-as.numeric(as.character(subsWT$year)) #make "year" readable to merge
  
  #change labels to show env over the last nUnits sprintf("%02d",0:30)
  labs<-c("year", paste(meas,sprintf("%02d",0:nUnits), sep="."))
  names(subsWT) <- labs
  
  # merge environment and response data, following code from Teller 2016
  datag <- merge(response, subsWT, by="year")
  #datag$year <- as.factor(datag$year); # why is year converted to a factor here?
  
  ## define and enter covariates the way the fitting function wants
  #tvars <- which(substr(names(datag),1,2)=="env."); 
  # much easier with dplyr:
  tvars <- dplyr::select(datag, starts_with(meas))
  datag <- dplyr::select(datag, -starts_with(meas))
  datag$envCovar <- as.matrix(tvars) 
  
  datag$lags <- matrix(1:ncol(tvars), nrow(datag), ncol(tvars), byrow = TRUE); 
#  for(i in 1:ncol(lags)) lags[,i]=i; 
#  datag$lags=as.matrix(lags); 
  
  return(datag)
}
