
source("assembleData_lags_Feb12.r") 

tot2003.1 <- allLags %>% filter(year == 2003) %>% summarize(cases2003 = sum(cases)) #1942
tot2012.1 <- allLags %>% filter(year == 2012) %>% summarize(cases2012 = sum(cases)) #193


allLags <- allLags[allLags$County != "Arthur" &
                     allLags$County != "Loup" &
                     allLags$County != "Sioux",]

tot2003.2 <- allLags %>% filter(year == 2003) %>% summarize(cases2003 = sum(cases)) #1942
tot2012.2 <- allLags %>% filter(year == 2012) %>% summarize(cases2012 = sum(cases)) #193


oosy <- max(allLags$year) # or specify 
allLags$County <- as.factor(allLags$County)
csco <- length(unique(allLags$County))
contrasts(allLags$County) = contr.sum(csco)
allLags <- allLags[allLags$year >= 2002,]

allLagsT <- allLags[allLags$year != oosy,] #trainng data
allLagsO <- allLags[allLags$year == oosy,]

library(gamm4)
library(lme4)
library(mgcv)
library(MuMIn)
library(broom)
library(purrr)


modname <- "M1112_Feb_ny1"
modform <- cases ~ s(lags_tmean30, by = tmean30) + s(lags_spei30, by = spei30) + CI + County + offset(log(pop100K))

mod <- gam(modform, data=allLagsT, family=nb())

modsum <- summary(mod)

AIC(mod)

source("allIndepData_noyr.r")

source("noyr_lags.r")
