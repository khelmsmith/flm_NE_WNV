#' Functional Linear Modeling for Vector-borne Disease
#' 
#' Use functional linear modeling statistical approach to predict mosquito-borne disease.
#' Developed in the context of West Nile virus in Nebraska
#' 
#' @docType package
#' @name flm.NE.WNV
NULL

# Code for building package (Run during development only)
#devtools::document()
#devtools::check()
#devtools::load_all()
#spelling::spell_check_packages()
#
# Save the .csv data as .rda for use in the package.
# Note the objects need to be read in and are saved with the same object name
#usethis::use_data(NE_county_pops)
#usethis::use_data(sampledat)
#usethis::use_data(NEdat)
#usethis::use_data(spi)
#usethis::use_data(spei)


# Document data sets
#' NE_county_pops
#'
#' Nebraska County populations, 2000-2019, from the U.S. Census Bureau, using annual estimates
#' @docType data
#'
"NE_county_pops"

#' sampledat
#'
#' simulated data on annual numbers of human cases of neuro-invasive and non-neuro-invasive
#' West Nile Virus in Nebraska counties. It is predictions of a model that was trained on
#' actual numbers of cases as recorded in CDC's Arbonet database. It excludes Arthur County,
#' because no cases have been recorded there to date, and we had to exclude it from our
#' modeling to get it to work.
#' @docType data
#'
"sampledat"

#' NEdat
#'
#' temperature and precipitation data for Nebraska counties each month from
#' January 1998 to February 2019 from the National Centers for Environmental
#' Information, National Climatic Data Center 
#' @docType data
#' @source ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/
#'
"NEdat"

#' spi
#'
#' Extracted monthly values from Westwide Drought Tracker netcdf files through
#' 2019-12-15. See Abatzoglou, J. T., McEvoy, D. J., & Redmond, K. T. (2017).
#' The West Wide Drought Tracker: Drought monitoring at fine spatial scales.
#' Bulletin of the American Meteorological Society, 98(9), 1815–1820.
#' https://doi.org/10.1175/BAMS-D-16-0193.1
#' @docType data
#'
"spi"

#' spei
#'
#' Extracted monthly values from Westwide Drought Tracker netcdf files through
#' 2019-12-15. See Abatzoglou, J. T., McEvoy, D. J., & Redmond, K. T. (2017).
#' The West Wide Drought Tracker: Drought monitoring at fine spatial scales.
#' Bulletin of the American Meteorological Society, 98(9), 1815–1820.
#' https://doi.org/10.1175/BAMS-D-16-0193.1
#' @docType data
#'
"spei"

# Identify dependencies used in dfmip (see also DESCRIPTION file)
# Set up package imports for NAMESPACE
#**# Importing specific functions using @importFrom is a better practice
#**# Watch for trouble with predict - it may be from one of the packages, and not stats
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @importFrom lubridate parse_date_time
#' @import readxl
#' @import broom
#' @import gamm4
#' @import lme4
#' @import mgcv
#' @importFrom MuMIn AICc
#' @import purrr
#' @import ggplot2
#' @importFrom stats contr.sum contrasts<- gaussian na.omit predict rnbinom sd update
#' @importFrom utils write.csv
NULL

#' Main function
#' 
#' @param pop County populations, a data frame with 5 variables \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{pop100K}, \preformatted{density}. 
#' @param cases data on annual numbers of human cases in each county. A data.frame with 3 variables, \preformatted{County}, \preformatted{year}, and \preformatted{cases}.
#' @param weather monthly temperature and precipitation data for for each county. A data.frame with  \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month}, \preformatted{tmean}, and \preformatted{ppt}.
#' @param spi monthly values of the Standardized Precipitation Index for each county. \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month}, \preformatted{spi}.
#' @param spei monthly values of the Standardized Precipitation and Evapotranspiration Index for each county. \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month}, \preformatted{spei}.
#' @param target.date The last date to include for calculation of lags, a character string with ISO XXX format (yyyy-mm-dd).
#' @param start.year The first year to include in the training data. Should be coercible to integer.
#' @param in.seed If not NULL, the starting number for the random number generator. This makes the results repeatable. If NULL, treats cases as actual data
#' @param lag.lengths the number of months to go backwards when creating lag matrices. Numeric vector.
#'
#' @export
call.flm = function(pop, cases, weather, spi, spei, target.date = "2018-02-01",
                    start.year = 2002, in.seed = NULL, lag.lengths = c(12, 18, 24, 30, 36)){
  checkInputs(pop, cases, weather, spi, spei, target.date, start.year, in.seed, lag.lengths)
  
  # Assemble data lags
  message("Assembling Data")
  start.time = Sys.time()
  adl.out = assemble.data.lags(pop, cases, weather, spi, spei, target.date, start.year, in.seed, lag.lengths = lag.lengths)
  allLagsT = adl.out[[1]]
  allLagsO = adl.out[[2]]
  message(sprintf("Elapsed Time: %.2f", Sys.time() - start.time))
  
  
  # Compare models with and without lags
  message("Comparing models with and without lags")

  models <- c("cases ~ s(lags_tmean%d, by=tmean%d) + County + year + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + CI + County + year + offset(log(pop100K))",
              
              "cases ~ s(lags_ppt%d, by=ppt%d) + County + year + offset(log(pop100K))",
              "cases ~ s(lags_ppt%d, by=ppt%d) + CI + County + year + offset(log(pop100K))",
              
              "cases ~ s(lags_spi%d, by=spi%d) + County + year + offset(log(pop100K))",
              "cases ~ s(lags_spi%d, by=spi%d) + CI + County + year + offset(log(pop100K))",
              
              "cases ~ s(lags_spei%d, by=spei%d) + County + year + offset(log(pop100K))",
              "cases ~ s(lags_spei%d, by=spei%d) + CI + County + year + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + County + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + CI + County + offset(log(pop100K))",
              
              "cases ~ s(lags_ppt%d, by=ppt%d) + County + offset(log(pop100K))",
              "cases ~ s(lags_ppt%d, by=ppt%d) + CI + County + offset(log(pop100K))",
              
              "cases ~ s(lags_spi%d, by=spi%d) + County + offset(log(pop100K))",
              "cases ~ s(lags_spi%d, by=spi%d) + CI + County + offset(log(pop100K))",
              
              "cases ~ s(lags_spei%d, by=spei%d) + County + offset(log(pop100K))",
              "cases ~ s(lags_spei%d, by=spei%d) + CI + County + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + County + year + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + CI + County + year + density + offset(log(pop100K))",
              
              "cases ~ s(lags_ppt%d, by=ppt%d) + County + year + density + offset(log(pop100K))",
              "cases ~ s(lags_ppt%d, by=ppt%d) + CI + County + year + density + offset(log(pop100K))",
              
              "cases ~ s(lags_spi%d, by=spi%d) + County + year + density + offset(log(pop100K))",
              "cases ~ s(lags_spi%d, by=spi%d) + CI + County + year + density + offset(log(pop100K))",
              
              "cases ~ s(lags_spei%d, by=spei%d) + County + year + density + offset(log(pop100K))",
              "cases ~ s(lags_spei%d, by=spei%d) + CI + County + year + density + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + County + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + CI + County + density + offset(log(pop100K))",
              
              "cases ~ s(lags_ppt%d, by=ppt%d) + County + density + offset(log(pop100K))",
              "cases ~ s(lags_ppt%d, by=ppt%d) + CI + County + density + offset(log(pop100K))",
              
              "cases ~ s(lags_spi%d, by=spi%d) + County + density + offset(log(pop100K))",
              "cases ~ s(lags_spi%d, by=spi%d) + CI + County + density + offset(log(pop100K))",
              
              "cases ~ s(lags_spei%d, by=spei%d) + County + density + offset(log(pop100K))",
              "cases ~ s(lags_spei%d, by=spei%d) + CI + County + density + offset(log(pop100K))")
  
  allmods_list <- map(lag.lengths,
                      ~sprintf(models, .x, .x))
  
  models <- c("cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + County + year + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + CI + County + year + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + County + year + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + CI + County + year + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + County + year + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + CI + County + year + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + County + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + CI + County + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + County + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + CI + County + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + County + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + CI + County + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + County + year + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + CI + County + year + density + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + County + year + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + CI + County + year + density + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + County + year + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + CI + County + year + density + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + County + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_ppt%d, by=ppt%d) + CI + County + density + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + County + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spi%d, by=spi%d) + CI + County + density + offset(log(pop100K))",
              
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + County + density + offset(log(pop100K))",
              "cases ~ s(lags_tmean%d, by=tmean%d) + s(lags_spei%d, by=spei%d) + CI + County + density + offset(log(pop100K))")
  
  lag_combinations <- crossing(tlag = lag.lengths, slag = lag.lengths)
  
  allmods_list2 <- map2(lag_combinations$tlag, lag_combinations$slag,
                        ~sprintf(models, .x, .x, .y, .y))
  
  allmods <- flatten(c(list("cases ~ County + year + offset(log(pop100K))",
                            "cases ~ CI + County + year + offset(log(pop100K))",
                            "cases ~ County + offset(log(pop100K))",
                            "cases ~ CI + County + offset(log(pop100K))",
                            "cases ~ County + year + density + offset(log(pop100K))",
                            "cases ~ CI + County + year + density + offset(log(pop100K))",
                            "cases ~ County + density + offset(log(pop100K))",
                            "cases ~ CI + County + density + offset(log(pop100K))"),
                       allmods_list, allmods_list2))
  
  process.start = Sys.time()

  results <- models_lags(allmods, allLagsT, allLagsO, results.path) 

  message(sprintf("Elapsed Time: %.2f; Process time: %.2f", (Sys.time() - start.time), (Sys.time() - process.start)))
  
  #**# Update when these are extracted in a format that can be passed to dfmip
  flm.results = results$predictions
  flm.distributions = NA
  flm.other = results$other
  
  # Return model results as a list
  #**# Need to return data in something that can be extracted by the update.df function
  #**# Need to return distribution if available - was that done? Otherwise, can use the point estimate from first output
  #**# Can return anything else as an additional output. This can be returned as a keyed list from dfmip #**# eventually
  return(list(flm.results, flm.distributions, flm.other))  
}
