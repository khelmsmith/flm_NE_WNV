## code to prepare `sampledat` dataset goes here
# make sampledat from fitted model
# assumes an input file with the fitted values in the cases column.
#sampledat <- readr::read_csv("data-raw/sampledat.csv")
sampledat <- readr::read_csv("data-raw/predictionsthrough2018.csv")
# fix arthur county with bayesian posterior for 16 samples of 0 from a poisson distribution
# given a 1, 1 gamma prior we have gamma(1, 17) as the posterior. This
# still has a probability 0.52 of observing 16 zeros at the median. 
# So the median is 0.04077336
sampledat <- dplyr::bind_rows(sampledat, dplyr::tibble(year = 2002:2019,
                             County = "Arthur",
                             pop100K = 0.00445,
                             cases = 0.0477336))
usethis::use_data(sampledat, overwrite = TRUE)
