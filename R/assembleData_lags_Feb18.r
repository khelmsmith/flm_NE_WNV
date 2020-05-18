#' Assemble human case data and environmental lags
#'
#' @param pop #**# Add documentation
#' @param cases #**# Add documentation
#' @param NEdat #**# Add documentation
#' @param spi #**# Add documentation
#' @param spei #**# Add documentation
#' @param target.date The last date to include for calculation of lags
#' @param start.year The first year to include in the training data
#' @param in.seed The starting number for the random number generator. This makes the results repeatable.
#' @export
assemble.data.lags = function(pop, cases, NEdat, spi, spei, target.date, start.year, in.seed){
  
  # Identify starting month #**# Should this be an input, or is it best to use the month from the target date?
  start.month = as.numeric(strsplit(target.date, '-')[[1]][2])
  
  # Assemble Human Cases data
  pop$year <- as.integer(pop$year)
  pop$cofips <- sprintf("%03d", pop$cofips)
  
  #fitted model has size = 5.129 so
  set.seed(in.seed) # pick a seed, any seed
  cases2 <- dplyr::mutate(cases, cases = rnbinom(length(cases), size = 5.129, mu = cases))
  HCcases <- dplyr::left_join(pop, cases2, by = c("County", "year"))
  
  # HCcases[is.na(HCcases)] <- 0
  
  HCcases <- HCcases[HCcases$year > 2001 & HCcases$year < 2018,]  
  
  # compute cumulative indicence
  
  HC <- HCcases %>%
    dplyr::group_by(County) %>%
      dplyr::mutate(CI = lag(cumsum(cases/pop100K)),
             Lcases = lag(cases))
  
  # incorporate ppt and temp
  
  NEdat$cofips <- sprintf("%03d", NEdat$cofips)
  NEdat <- dplyr::left_join(NEdat, HC[,c("cofips", "County")])
  NEdat <- dplyr::mutate(NEdat, yrmo = paste(year, month))
  NEdat$yrmo <- parse_date_time(NEdat$yrmo, "ym")
  NEdat <- NEdat[NEdat$yrmo <= target.date,]

  #summarize by month for making lags
  NEdat <- NEdat %>%
    dplyr::group_by(County, year, month) %>%
    dplyr::summarize(tmean = mean(temp), ppt = sum(ppt))
  
  #standardize temperature 
  NEdat <- NEdat %>% dplyr::group_by(County, month) %>% 
    dplyr::mutate(sdtmean = (tmean - mean(tmean)) /sd(tmean))
  
  #standardize precipitation
  NEdat <- NEdat %>% dplyr::group_by(County, month) %>% 
    dplyr::mutate(sdppt = (ppt - mean(ppt)) / sd(ppt))
  
  NEdat <- as.data.frame(NEdat)
  
  # West Wide Drought Tracker data
  
  # spi
  spi <- spi %>%
    dplyr::mutate(YrMo=paste(year, month, sep="-"))
  spi$YrMo <- parse_date_time(spi$YrMo, "ym")
  spi$YrMo <- as.Date(spi$YrMo)
  spi <- spi[spi$YrMo <= target.date,]
  spi <- dplyr::rename(spi, spi = spi1)
  spi$County <- trimws(spi$County)
  spi$County <- gsub(" ", "", spi$County)
  # spi <- spi[,c(2:3,6:7)]
  
  #spei
  
  spei <- spei %>%
    dplyr::mutate(YrMo=paste(year, month, sep="-"))
  spei$YrMo <- parse_date_time(spei$YrMo, "ym")
  spei$YrMo <- as.Date(spei$YrMo)
  spei <- spei[spi$YrMo <= target.date,]
  spei <- rename(spei, spei = spei1)
  spei$County <- trimws(spei$County)
  spei$County <- gsub(" ", "", spei$County)
  
  #load lag maker
  
  #source("R/teller_2016_make_data.R")
  
  #Fully automated lag-looping 
  
  #enter the number of months of lags that you'd like to make
  lagLengths <- c(12, 18, 24, 30, 36) #**# Should this be an optional input?
  #enter the number of different lags you are making
  listOfLagLengths <- vector("list", length = 5)
  names(listOfLagLengths) <- lagLengths
  
  #enter the number of variables you are lagging
  listofVars <- vector("list", length=4) #as length
  names(listofVars) <- 1:4 #and as number of list items
  
  lagnames <- unique(HC$County)
  listOfLags <- vector("list", length=length(lagnames))
  names(listOfLags) <- lagnames
  
  
  for (j in seq_along(listOfLagLengths)) {
    nUnits <- lagLengths[[j]]
    
    #spi lags
    for (i in seq_along(lagnames)) {
      County.i <- lagnames[[i]]
      spi.i <- filter(spi, County == County.i)
      dry.i <- select(spi.i, year, month, spi) 
      HC.i <- filter(HC, County == County.i)
      HC.i <- select(HC.i, County, year, cases, Lcases, CI, pop100K) 
      lags.i <- makeDat(dry.i, HC.i, start.month, "spi", nUnits) #the "2" means "February. Pick your start month.
      lags.i$County <- County.i 
      listOfLags[[i]] <- lags.i 
    }
    
    rows <- do.call(rbind, listOfLags)
    names(rows)[names(rows)== "envCovar"] <- paste0("spi",nUnits) 
    names(rows)[names(rows)=="lags"] <- paste0("lags_spi",nUnits)
    
    listofVars[[1]] <- rows
    
    #spei lags
    for (i in seq_along(lagnames)) {
      County.i <- lagnames[[i]]
      spei.i <- dplyr::filter(spei, County == County.i)
      dry.i <- dplyr::select(spei.i, year, month, spei) 
      HC.i <- dplyr::filter(HC, County == County.i)
      HC.i <- dplyr::select(HC.i, County, year, cases, Lcases, CI, pop100K) 
      lags.i <- makeDat(dry.i, HC.i, start.month, "spei", nUnits) #the "2" means "February. Pick your start month.
      lags.i$County <- County.i 
      listOfLags[[i]] <- lags.i 
    }
    
    rows <- do.call(rbind, listOfLags)
    names(rows)[names(rows)== "envCovar"] <- paste0("spei",nUnits)
    names(rows)[names(rows)=="lags"] <- paste0("lags_spei",nUnits)
    
    listofVars[[2]] <- rows
    
    #sdppt
    
    #looping
    for (i in seq_along(lagnames)) {
      County.i <- lagnames[[i]]
      x.i <- dplyr::filter(NEdat, County == County.i)
      ppt.i <- dplyr::select(x.i, year, month, sdppt) 
      HC.i <- dplyr::filter(HC, County == County.i)
      HC.i <- dplyr::select(HC.i, County, year, cases, Lcases, CI, pop100K) 
      lags.i <- makeDat(x.i, HC.i, start.month, "sdppt", nUnits) #the "2" means "February. Pick your start month.
      lags.i$County <- County.i 
      listOfLags[[i]] <- lags.i 
    }
    
    rows <- do.call(rbind, listOfLags)
    names(rows)[names(rows)== "envCovar"] <- paste0("ppt",nUnits)
    names(rows)[names(rows)=="lags"] <- paste0("lags_ppt",nUnits)
    
    listofVars[[3]] <- rows
    
    #sdtmean
    
    #looping
    for (i in seq_along(lagnames)) {
      County.i <- lagnames[[i]]
      x.i <- dplyr::filter(NEdat, County == County.i)
      tmean.i <- dplyr::select(x.i, year, month, sdtmean) 
      HC.i <- dplyr::filter(HC, County == County.i)
      HC.i <- dplyr::select(HC.i, County, year, cases, Lcases, CI, pop100K) 
      lags.i <- makeDat(x.i, HC.i, start.month, "sdtmean", nUnits) #the "2" means "February. Pick your start month.
      lags.i$County <- County.i 
      listOfLags[[i]] <- lags.i 
    }
    
    rows <- do.call(rbind, listOfLags)
    names(rows)[names(rows)== "envCovar"] <- paste0("tmean",nUnits)
    names(rows)[names(rows)=="lags"] <- paste0("lags_tmean",nUnits)
    
    listofVars[[4]] <- rows
    
    merge.all <- function(x, y) {
      merge(x, y, all=TRUE, by=c("County", "year", "cases", "Lcases", "CI", "pop100K"))
    }
    
    listOfLagLengths[[j]] <- Reduce(merge.all, listofVars)
    
  }
  
  allLags <- Reduce(merge.all, listOfLagLengths)

  # Moved from predict_nYr.r. Looked like it was duplicated and also used by predict_wYr.r and WNV_model_lags.R
  oosy <- max(allLags$year) # or specify
  allLags$County <- as.factor(allLags$County)
  csco <- length(unique(allLags$County))
  contrasts(allLags$County) = contr.sum(csco)
  allLags <- allLags[allLags$year >= start.year,]
  
  allLagsT <- allLags[allLags$year != oosy,] #training data
  yrmin <- min(allLagsT$year)
  yrmax <- max(allLagsT$year)
  allLagsT$year <- as.factor(allLagsT$year)
  csyr <- length(unique(allLagsT$year))
  contrasts(allLagsT$year) = contr.sum(csyr)
  
  allLagsO <- allLags[allLags$year == oosy,] # out-of-sample data
  
  return(list(allLagsT, allLagsO))
}

