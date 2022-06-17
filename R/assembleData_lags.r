#' Assemble human case data and environmental lags
#' 
#' \code{assemble.data.lags()} expects inputs to follow certain conventions (see below). 
#' 
#' Case data are trimmed to between \preformatted{start.year}
#' and \preformatted{target.year}. predictor variables are trimmed to between January of \preformatted{start.year}
#' and the date specified in \preformatted{target.year}.
#'
#' The \preformatted{County} variable can be any character string unique to each geographic locale, as long as the format is 
#' identical across all input data frames.
#' Simulated data (in.seed not null) is predictions of a model that was trained on actual numbers of cases as recorded in CDC's Arbonet database. It excludes Arthur County, because no cases have been recorded there to date, and we had to exclude it from our modeling to get it to work. 
#' Weather data included in package for Nebraska comes from National Centers for Environmental Information, National Climatic Data Center (ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/).
#' SPEI and SPI from Westwide Drought Tracker netcdf files. See Abatzoglou, J. T., McEvoy, D. J., & Redmond, K. T. (2017). The West Wide Drought Tracker: Drought monitoring at fine spatial scales. Bulletin of the American Meteorological Society, 98(9), 1815–1820. https://doi.org/10.1175/BAMS-D-16-0193.1.
#'
#' @return a list of two data frames, one with training data and one with the 
#' target year data. 
#'
#' @param pop County populations, a data frame with 5 variables \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{pop100K}, \preformatted{density}. 
#' @param cases data on annual numbers of human cases in each county. A data.frame with 3 variables, \preformatted{County}, \preformatted{year}, and \preformatted{cases}.
#' @param weather monthly temperature and precipitation data for for each county. A data.frame with  \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month} (integer), \preformatted{tmean}, and \preformatted{ppt}.
#' @param spi monthly values of the Standardized Precipitation Index for each county. \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month} (integer), \preformatted{spi}.
#' @param spei monthly values of the Standardized Precipitation and Evapotranspiration Index for each county. \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month} (integer), \preformatted{spei}.
#' @param target.date The last date to include for calculation of lags, a character string with ISO XXX format (yyyy-mm-dd).
#' @param start.year The first year to include in the training data. Should be coercible to integer.
#' @param in.seed If not NULL, the starting number for the random number generator. This makes the results repeatable. If NULL, treats cases as actual data
#' @param lag.lengths the number of months to go backwards when creating lag matrices. Numeric vector.
#' @export
assemble.data.lags = function(pop, cases, weather, spi, spei, target.date, start.year, in.seed = NULL,
                              lag.lengths = c(12, 18, 24, 30, 36)){
  # Identify starting month #**# Should this be an input, or is it best to use the month from the target date?
  start.month = as.numeric(strsplit(target.date, '-')[[1]][2])
  target.year = as.numeric(strsplit(target.date, '-')[[1]][1])
  
  
  # Assemble Human Cases data

  # simulate data if in.seed not null
  if(!is.null(in.seed)){
    #fitted model has size = 5.129 so
    set.seed(in.seed) # pick a seed, any seed
    cases2 <- dplyr::mutate(cases, cases = as.integer(rnbinom(length(cases), size = 5.129, mu = cases)))
    HCcases <- dplyr::left_join(pop, cases2, by = c("County", "year"))
  } else {
    # check that cases has the right variable
    if(!("cases" %in% names(cases))){
      stop("ensure cases data frame has a cases variable")
    }
    HCcases <- dplyr::left_join(pop, cases, by = c("County", "year"))
  }
  
  HCcases[is.na(HCcases)] <- 0L
  HCcases <- HCcases[HCcases$year >= start.year & HCcases$year <= target.year, ]  
  
  # compute cumulative incidence and lagged cases
  
  HC <- HCcases %>%
    dplyr::group_by(County) %>%
    dplyr::mutate(total_cases = sum(cases)) %>% 
#    dplyr::filter(total_cases > 0L) %>% 
      dplyr::mutate(CI = lag(cumsum(cases/pop100K)),
             Lcases = lag(cases)) %>% 
    dplyr::mutate(CI = case_when(is.na(CI)~0,
                                 TRUE~CI),
                  Lcases = case_when(is.na(Lcases)~0L,
                                     TRUE~Lcases)) %>% 
    dplyr::ungroup() %>% 
    as.data.frame()
  
  # incorporate ppt and temp
  
  weather <- dplyr::mutate(weather, yrmo = paste(year, month))
  weather$yrmo <- parse_date_time(weather$yrmo, "ym")
  weather <- weather[weather$yrmo <= target.date,]

  # standardize temperature and precipitation
  weather <- weather %>% 
    dplyr::group_by(County, month) %>% 
    dplyr::mutate(sdtmean = (tmean - mean(tmean))/sd(tmean),
                  sdppt = (ppt - mean(ppt))/sd(ppt)) %>% 
    as.data.frame()

  # West Wide Drought Tracker data
  
  # spi
  spi <- spi %>%
    dplyr::mutate(YrMo=paste(year, month, sep="-"))
  spi$YrMo <- lubridate::ym(spi$YrMo)
#  spi$YrMo <- as.Date(spi$YrMo)
  spi <- spi[spi$YrMo <= target.date,]
#  spi <- dplyr::rename(spi, spi = spi1)

  #spei
  spei <- spei %>%
    dplyr::mutate(YrMo=paste(year, month, sep="-"))
  spei$YrMo <- parse_date_time(spei$YrMo, "ym")
  spei$YrMo <- as.Date(spei$YrMo)
  spei <- spei[spei$YrMo <= target.date,]

  #Fully automated lag-looping 
  

  #enter the number of different lags you are making
  listOfLagLengths <- vector("list", length = length(lag.lengths))
  names(listOfLagLengths) <- lag.lengths
  
  #enter the number of variables you are lagging
  listofVars <- vector("list", length=4) #as length
  names(listofVars) <- 1:4 #and as number of list items
  
  lagnames <- unique(HC$County)
  listOfLags <- vector("list", length=length(lagnames))
  names(listOfLags) <- lagnames
  
  
  for (j in seq_along(listOfLagLengths)) {
    nUnits <- lag.lengths[[j]]
    
    #spi lags
    for (i in seq_along(lagnames)) {
      County.i <- lagnames[[i]]
      spi.i <- dplyr::filter(spi, County == County.i)
      dry.i <- dplyr::select(spi.i, year, month, spi) 
      HC.i <- dplyr::filter(HC, County == County.i)
      HC.i <- dplyr::select(HC.i, County, year, cases, Lcases, CI, pop100K, density) 
      lags.i <- makeDat(dry.i, HC.i, start.month, "spi", nUnits) #the "2" means "February. Currently generated from target.date.
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
      HC.i <- dplyr::select(HC.i, County, year, cases, Lcases, CI, pop100K, density) 
      lags.i <- makeDat(dry.i, HC.i, start.month, "spei", nUnits) #the "2" means "February. Currently generated from target.date.
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
      x.i <- dplyr::filter(weather, County == County.i)
      ppt.i <- dplyr::select(x.i, year, month, sdppt) 
      HC.i <- dplyr::filter(HC, County == County.i)
      HC.i <- dplyr::select(HC.i, County, year, cases, Lcases, CI, pop100K, density) 
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
      x.i <- dplyr::filter(weather, County == County.i)
      tmean.i <- dplyr::select(x.i, year, month, sdtmean) 
      HC.i <- dplyr::filter(HC, County == County.i)
      HC.i <- dplyr::select(HC.i, County, year, cases, Lcases, CI, pop100K, density) 
      lags.i <- makeDat(x.i, HC.i, start.month, "sdtmean", nUnits) #the "2" means "February. Pick your start month.
      lags.i$County <- County.i 
      listOfLags[[i]] <- lags.i 
    }
    
    rows <- do.call(rbind, listOfLags)
    names(rows)[names(rows)== "envCovar"] <- paste0("tmean",nUnits)
    names(rows)[names(rows)=="lags"] <- paste0("lags_tmean",nUnits)
    
    listofVars[[4]] <- rows
    
    merge.all <- function(x, y) {
      merge(x, y, all=TRUE, by=c("County", "year", "cases", "Lcases", "CI", "pop100K", "density"))
    }
    
    listOfLagLengths[[j]] <- Reduce(merge.all, listofVars)
    
  }
  
  allLags <- Reduce(merge.all, listOfLagLengths)

  # Moved from predict_nYr.r. Looked like it was duplicated and also used by predict_wYr.r and WNV_model_lags.R
  # oosy <- max(allLags$year) # Use target.year
  # before building county contrast, ensure counties with no cases prior to 
  # target.year also remove counties in target.year
  counties_w_cases <- allLags[allLags$year != target.year,] 
  counties_w_cases <- dplyr::group_by(counties_w_cases, County)
  counties_w_cases <- dplyr::mutate(counties_w_cases, total_cases = sum(cases))
  counties_w_cases <- unique(counties_w_cases[counties_w_cases$total_cases > 0,"County", drop = TRUE])
  # save the target year data for unpredicted counties
  unpredictedO <- allLags[!(allLags$County %in% counties_w_cases) & 
                            allLags$year == target.year, ]
  allLags <- allLags[allLags$County %in% counties_w_cases, ]
    
  allLags$County <- as.factor(allLags$County)
  csco <- length(unique(allLags$County))
  contrasts(allLags$County) = contr.sum(csco)
  allLags <- allLags[allLags$year >= start.year,]
  
  allLagsT <- allLags[allLags$year != target.year,] #training data
  
  # yrmin <- min(allLagsT$year)
  # yrmax <- max(allLagsT$year)
  allLagsT$year <- as.factor(allLagsT$year)
  csyr <- length(unique(allLagsT$year))
  contrasts(allLagsT$year) = contr.sum(csyr)
  
  allLagsO <- allLags[allLags$year == target.year,] # out-of-sample data
  
  return(list(allLagsT, allLagsO, unpredictedO))
}

