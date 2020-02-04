## Assemble and set up data if it isn't already done, using the chunk below

source("code/assembleData_lags_Feb18.r")

oosy <- max(allLags$year) # or specify
allLags$County <- as.factor(allLags$County)
csco <- length(unique(allLags$County))
contrasts(allLags$County) = contr.sum(csco)
allLags <- allLags[allLags$year >= 2002,]

allLagsT <- allLags[allLags$year != oosy,] #training data
# yrmin <- min(allLagsT$year)
# yrmax <- max(allLagsT$year)
# allLagsT$year <- as.factor(allLagsT$year)
# csyr <- length(unique(allLagsT$year))
# contrasts(allLagsT$year) = contr.sum(csyr)

allLagsO <- allLags[allLags$year == oosy,] # out-of-sample data

library(gamm4)
library(lme4)
library(mgcv)
library(MuMIn)
library(broom)
library(purrr)

# plug in model formula here
modform <- cases ~ s(lags_tmean12, by = tmean12) + s(lags_ppt12, by = ppt12) + CI + County + offset(log(pop100K))

mod <- gam(modform, data=allLagsT, family=nb())

modsum <- summary(mod)

# predict for years included in training data

allYears <- unique(allLagsT$year)
listofallyears <- vector("list", length = length(allYears))
names(listofallyears) <- allYears

for (i in seq_along(allYears)) {
  yr.i <- allYears[i]
  indepData.i <- allLagsT[allLagsT$year != yr.i,]
  M.i <- gam(modform, data=indepData.i, family=nb())
  yrdata.i <- allLagsT[allLagsT$year == yr.i,]
  predyr.i <- predict(M.i, newdata=yrdata.i, type = "link", se=TRUE)
  # yrdata.i$testcol <- NULL
  yrdata.i$fit <- predyr.i[[1]]
  yrdata.i$se <- predyr.i[[2]]
  yrdata.i <- yrdata.i[,c("County", "year", "cases", "Lcases", "fit", "se")]
  listofallyears[[i]] <- yrdata.i
}

allIndepDataT <- do.call(rbind, listofallyears)

library(dplyr)
library(broom)
library(dplyr)
library(tidyr)

# predict for out-of-sample data

predsO <- predict(mod, newdata=allLagsO, type = "link", se=TRUE)
allLagsO$fit <- predsO[[1]]
allLagsO$se <- predsO[[2]]
allLagsO <- allLagsO[,c("County", "year", "cases", "Lcases", "fit", "se")]

allIndepData <- rbind(allIndepDataT, allLagsO)

# exponentiate fit to get predictions from model with log link

allIndepData <- mutate(allIndepData, pred = exp(fit))

