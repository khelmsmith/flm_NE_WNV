#' check inputs for consistency
#'
#' @param pop County populations, a data frame with 3 variables \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{pop100K} 
#' @param cases data on annual numbers of human cases in each county. A data.frame with 3 variables, \preformatted{County}, \preformatted{year}, and \preformatted{cases}.
#' @param weather monthly temperature and precipitation data for for each county. A data.frame with  \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month}, \preformatted{tmean}, and \preformatted{ppt}.
#' @param spi monthly values of the Standardized Precipitation Index for each county. \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month}, \preformatted{spi}.
#' @param spei monthly values of the Standardized Precipitation and Evapotranspiration Index for each county. \preformatted{County}, \preformatted{fips} (5 \strong{characters}), \preformatted{year}, \preformatted{month}, \preformatted{spei}.
#' @param target.date The last date to include for calculation of lags, a character string with ISO XXX format (yyyy-mm-dd).
#' @param start.year The first year to include in the training data. Should be coercible to integer.
#' @param in.seed If not NULL, the starting number for the random number generator. This makes the results repeatable. If NULL, treats cases as actual data
#' @param lag.lengths the number of months to go backwards when creating lag matrices. Numeric vector.
#' @export
checkInputs = function(pop, cases, weather, spi, spei, target.date, start.year, in.seed = NULL,
                              lag.lengths = c(12, 18, 24, 30, 36)){
  checkvariables <- function(x, expected){
    check <- names(x) %in% expected
    nameofx <- deparse(substitute(x))
    if(!all(check)){
      cat(paste(paste(expected), "\n"))
      stop(paste("data.frame",nameofx,"has issues. Check variables:", paste(names(x)[!check], sep = ","),"\n"))
    }
    if ("year" %in% expected){
      if (lubridate::year(target.date) > max(x$year)){
        stop(paste("data.frame",nameofx, "doesn't have enough data for target date ", target.date))
      }
    }
    if ("month" %in% expected){
      dates <- lubridate::ym(paste(x$year, x$month, sep = "-"))
      if (lubridate::ymd(target.date) > max(dates)){
        stop(paste("data.frame",nameofx,"doesn't have enough data for target date"))
      }
    }
  }
  checkvariables(pop, c("County","fips","year","pop100K", "density"))

  checkvariables(cases, c("County","year","cases"))

  HCcases <- dplyr::anti_join(pop, cases, by = c("County", "year"))
  if(nrow(HCcases)>0) stop("Check that County and year variables match between pop and cases data frames.")

  checkvariables(weather, c("County","fips","year","month","tmean","ppt"))
  weather_join_check <- dplyr::anti_join(pop, weather, by = c("County", "year"))
  if(nrow(weather_join_check)>0) stop("Check that County and year variables match between pop and weather data frames.")
  checkvariables(spi, c("County","fips","year","month","spi"))
  spi_join_check <- dplyr::anti_join(pop, spi, by = c("County", "year"))
  if(nrow(spi_join_check)>0) stop("Check that County and year variables match between pop and spi data frames.")
  checkvariables(spei, c("County","fips","year","month","spei"))
  spei_join_check <- dplyr::anti_join(pop, spei, by = c("County", "year"))
  if(nrow(spei_join_check)>0) stop("Check that County and year variables match between pop and spei data frames.")
  return(TRUE)
}