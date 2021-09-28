suppressPackageStartupMessages(library(dplyr))

# Read in population data
pop = flm.NE.WNV::NE_county_pops  %>% 
  filter(year >= 2002) %>% 
  as.data.frame()
 
county_lookup <- pop %>% 
  filter(year == "2010") %>% 
  select(County, fips)%>% 
  as.data.frame()

#Human cases, simulated, based on predicted values from the fitted model. 
cases = flm.NE.WNV::sampledat %>% 
  select(County, year, cases)%>% 
  as.data.frame()

# Read in environmental data
NEdat = flm.NE.WNV::NEdat %>% 
  rename(tmean = temp) %>% 
  mutate(fips = sprintf("31%03d", cofips)) %>% 
  left_join(county_lookup, by = "fips") %>% 
  select(County, fips, year, month, tmean, ppt)%>% 
  as.data.frame()
  

# Read in spi
spi = flm.NE.WNV::spi %>% 
  mutate(County = trimws(County),
         County = gsub(" ", "", County)) %>% 
  left_join(county_lookup, by = "County") %>% 
  rename(spi = spi1) %>% 
  select(County, fips, year, month, spi)%>% 
  as.data.frame()

# Read in spei
spei = flm.NE.WNV::spei %>% 
  mutate(County = trimws(County),
         County = gsub(" ", "", County)) %>% 
  left_join(county_lookup, by = "County") %>% 
  rename(spei = spei1) %>% 
  select(County, fips, year, month, spei)%>% 
  as.data.frame()

# Specify the last date to use
target.date = "2004-05-01" 

# Specify the first year of data to use
start.year = 2002

# Specify lag lengths to use
lag.lengths = c(9, 12) # default is c(12, 18, 24, 30, 36)

#For consistent results with simulated data
in.seed = 4872957

test_that(
  "inputs check out",{
  expect_true(checkInputs(pop, cases, NEdat, spi, spei, 
                          target.date, start.year, in.seed, lag.lengths))
  }
)

data <- assemble.data.lags(pop, cases, NEdat, spi, spei, 
                           target.date, start.year, in.seed, lag.lengths)

test_that(
  "return value has correct structure",{
  expect_length(data, 2)
  expect_null(names(data))
  expect_equal(names(data[[1]]), c("County", "year", "cases", "Lcases", "CI", "pop100K","density", "spi9", 
                                   "lags_spi9", "spei9", "lags_spei9", "ppt9", "lags_ppt9", "tmean9", 
                                   "lags_tmean9", "spi12", "lags_spi12", "spei12", "lags_spei12", 
                                   "ppt12", "lags_ppt12", "tmean12", "lags_tmean12"))
  expect_equal(dim(data[[1]]), c(164, 23))
  expect_equal(dim(data[[1]][,13]), c(164, 10))
  expect_equal(dim(data[[1]][,17]), c(164, 13))
  expect_equal(names(data[[2]]), c("County", "year", "cases", "Lcases", "CI", "pop100K","density", "spi9", 
                                   "lags_spi9", "spei9", "lags_spei9", "ppt9", "lags_ppt9", "tmean9", 
                                   "lags_tmean9", "spi12", "lags_spi12", "spei12", "lags_spei12", 
                                   "ppt12", "lags_ppt12", "tmean12", "lags_tmean12"))
  expect_equal(dim(data[[2]]), c(82, 23))
  expect_equal(dim(data[[2]][,13]), c(82, 10))
  expect_equal(dim(data[[2]][,17]), c(82, 13))
  }
)
spi_test1 <- spi %>% 
  mutate(yrmo = lubridate::ym(paste(year, month, sep = "-")),
         County = trimws(County)) %>% 
  filter(County == "Adams",
         yrmo >= "2001-08-01",
         yrmo <= "2002-05-01") %>% 
  pull(spi) %>% 
  rev()

spi_test2 <- spi %>% 
  mutate(yrmo = lubridate::ym(paste(year, month, sep = "-")),
         County = trimws(County)) %>% 
  filter(County == "Adams",
         yrmo >= "2003-08-01",
         yrmo <= "2004-05-01") %>% 
  pull(spi) %>% 
  rev()
  
test_that("spi has correct values",{
  expect_equal(data[[1]][1,8], spi_test1, ignore_attr = TRUE)
  expect_equal(data[[2]][1,8], spi_test2, ignore_attr = TRUE)
})
