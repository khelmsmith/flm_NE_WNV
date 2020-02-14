## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----data----------------------------------------------------------------
#**# Moving code from assembleData_lags file

# Read in population data
pop = flm::NE_county_pops
# pop <- read.csv("data/NE_county_pops.csv", stringsAsFactors = FALSE)

#Human cases. In this example, cases is the rounded value of the fitted model (i.e. simulated)
cases = flm::sampledat
# cases <- read.csv("data/sampledat.csv", stringsAsFactors = FALSE)

# Read in environmental data
NEdat = flm::NEdat
# NEdat <- read.csv("data/NEdat.csv", stringsAsFactors = FALSE)

# Read in spi
spi = flm::spi
# spi <- read.csv("data/NEco_spi1_2019-12-15.csv", stringsAsFactors = FALSE)

# Read in spei
spei = flm::spei
# spei <- read.csv("data/NEco_spei1_2019-12-15.csv", stringsAsFactors = FALSE)

# Specify the last date to use
target.date = "2018-02-01"

# Specify the first year of data to use
start.year = 2002

# Specify where to put the results
results.path = "temp/"


## ----flm-----------------------------------------------------------------

#**# Turned off function call while checking that the package loads
#flm.results = flm::call.flm(pop, cases, NEdat, spi, spei, target.date, start.year, results.path, in.seed = 4872957)

# Examine flm.results


