## Assemble and set up data

source("R/assembleData_lags_Feb18.r")

oosy <- max(allLags$year) # or specify
allLags$County <- as.factor(allLags$County)
csco <- length(unique(allLags$County))
contrasts(allLags$County) = contr.sum(csco)
allLags <- allLags[allLags$year >= 2002,]

allLagsT <- allLags[allLags$year != oosy,] #training data
yrmin <- min(allLagsT$year)
yrmax <- max(allLagsT$year)
allLagsT$year <- as.factor(allLagsT$year)
csyr <- length(unique(allLagsT$year))
contrasts(allLagsT$year) = contr.sum(csyr)

allLagsO <- allLags[allLags$year == oosy,] # out-of-sample data

library(gamm4)
library(lme4)
library(mgcv)
library(MuMIn)
library(broom)
library(purrr)

# Plug in the model formula here
modform <- cases ~ s(lags_tmean30, by = tmean30) + s(lags_spi24, by = spi24) + CI + County + year + offset(log(pop100K))

mod <- gam(modform, data=allLagsT, family=nb())

modsum <- summary(mod)

ptab <- as.data.frame(modsum$p.table)

years<-  grepl("year", row.names(ptab))
ptab <- ptab[years, 1, drop=FALSE]
colnames(ptab) <- "yrcoef"
ptab[nrow(ptab)+1,"yrcoef"] <- -sum(ptab$yrcoef)

ptab$year <- as.factor(c(yrmin:yrmax))

allLagsT <- merge(allLagsT, ptab) 

# predict the year coefficient with training data 

# change tmean and SP(E)I variable lag lengths here to match model formula

M1 <- gam(yrcoef ~ s(lags_tmean30, by = tmean30) + s(lags_spi24, by = spi24) + County, data=allLagsT, family=gaussian())

# use the year coefficient to predict coyr for each county-year on training data
new <- predict(M1, newdata = allLagsT, type = "link", se = TRUE)
allLagsT$coyr <- new$fit
allLagsT$coyrse <- new$se.fit
allLagsT$yrcoef <- NULL

#refit the model with training data
mod2 <- gam(update(modform, . ~ . - year + coyr), data=allLagsT, family=nb())
# summary(mod2)

library(dplyr)
library(broom)
library(dplyr)
library(tidyr)

# now predict using the new coyr variable instead of the year coefficient

# first for training data

allLagsT$year <- as.factor(allLagsT$year)
allYears <- levels(allLagsT$year)
listofallyears <- vector("list", length = length(allYears))
names(listofallyears) <- allYears
cs <- length(allYears)

for (i in seq_along(allYears)) {
  yr.i <- allYears[i]
  indepData.i <- allLagsT[allLagsT$year != yr.i,]
  indepData.i$year <- droplevels(indepData.i$year)
  contrasts(indepData.i$year) = contr.sum(cs-1)
 mod2 <- gam(update(modform, . ~ . - year + coyr), data = indepData.i, family=nb())

yrdata.i <- allLagsT[allLagsT$year == yr.i,]
predyr.i <- predict(mod2, newdata=yrdata.i, type = "link", se=TRUE) #mod2 is already created
  yrdata.i$fit <- predyr.i[[1]]
  yrdata.i$se <- predyr.i[[2]]
  yrdata.i <- yrdata.i[,c("County", "year", "cases", "Lcases", "fit", "se")]
  row.names(yrdata.i) <- NULL
  listofallyears[[i]] <- yrdata.i
}
allIndepDataT <- do.call(rbind, listofallyears)

# then for out-of-sample data

new <- predict(M1, newdata = allLagsO, type = "link", se = TRUE)
allLagsO$coyr <- new$fit
allLagsO$coyrse <- new$se.fit

preds <- predict(mod2, newdata=allLagsO, type = "link", se=TRUE) 
  allLagsO$fit <- preds[[1]]
  allLagsO$se <- preds[[2]]
  allLagsO <- allLagsO[,c("County", "year", "cases", "Lcases", "fit", "se")]

# combine them   

allIndepDataT$year <- as.character(allIndepDataT$year)
allIndepData <- rbind(allIndepDataT, allLagsO)

# exponentiate fit to get predictions from log-linked model

allIndepData <- mutate(allIndepData, pred = exp(fit))
