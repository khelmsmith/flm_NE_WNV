## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----data, message=FALSE------------------------------------------------------
library(tidyverse)
library(flm.NE.WNV) 

# Read in population data
pop = flm.NE.WNV::NE_county_pops %>% mutate(pop100K = pop/1e5) 

#Human cases, simulated, based on predicted values from the fitted model. 
cases = flm.NE.WNV::sampledat

# Read in environmental data
NEdat = flm.NE.WNV::NEdat

# Read in spi
spi = flm.NE.WNV::spi

# Read in spei
spei = flm.NE.WNV::spei

# Specify the last date to use
target.date = "2018-02-01" 

# Specify the first year of data to use
start.year = 2002

# Specify lag lengths to use
lag.lengths = c(9) # default is c(12, 18, 24, 30, 36)

#For consistent results with simulated data
in.seed = 4872957


## -----------------------------------------------------------------------------
data <- assemble.data.lags(pop, cases, NEdat, spi, spei, 
                           target.date, start.year, in.seed, lag.lengths)
allLagsT <- data[[1]] # 2004 - 2017
allLags0 <- data[[2]]
# how many counties lack cases? Should be none.
allLagsT %>% group_by(County) %>% 
  summarize(total_cases = sum(cases)) %>% 
  filter(total_cases == 0)

allLagsT %>% group_by(County) %>% 
  summarize(total_cases = sum(cases)) %>% 
  filter(total_cases > 0)

## -----------------------------------------------------------------------------
allLagsT %>% group_by(year) %>% 
  summarize(total_cases = sum(cases)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = year, y = total_cases))

## ----flm----------------------------------------------------------------------

flm.results = flm::call.flm(pop, cases, NEdat, spi, spei, target.date, start.year, in.seed, lag.lengths)


## -----------------------------------------------------------------------------
top <- flm.results[[3]]$best

mod <- summary(flm.results[[3]]$fittedModels[[top]])
mod


## -----------------------------------------------------------------------------
flm.results[[1]]

## -----------------------------------------------------------------------------
dist_lag_terms <- flm:::extract_functional(flm.results[[3]]$fittedModels[[top]]) %>% 
  mutate(lcl = fit - 1.96*se,
         ucl = fit + 1.96*se)
ggplot(data = dist_lag_terms) + 
  geom_line(mapping = aes(x = x, y = fit)) +
  geom_ribbon(mapping = aes(x = x, ymin = lcl, ymax = ucl), alpha = 0.2)+
  facet_wrap(~label)

