# assemble data through latest year (2018)

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(readxl)

# assemble Human Cases data

pop <- read.csv("data/NE_county_pops.csv", stringsAsFactors = FALSE)
pop$year <- as.integer(pop$year)
pop$cofips <- sprintf("%03d", pop$cofips)

#cases here is the rounded value of the fitted model
cases <- read.csv("data/sampledat.csv", stringsAsFactors = FALSE)
#fitted model has size = 5.129 so
set.seed(4872957) # pick a seed, any seed
cases2 <- mutate(cases, cases = rnbinom(length(cases), size = 5.129, mu = cases))
HCcases <- left_join(pop, cases2, by = c("County", "year"))

HCcases[is.na(HCcases)] <- 0

# compute cumulative indicence

HCcases <- mutate(HCcases, pop100K=pop/100000)
HC <- HCcases %>%
  group_by(County) %>%
    mutate(CI = lag(cumsum(cases/pop100K)),
           Lcases = lag(cases))

# incorporate ppt and temp

NEdat <- read.csv("data/NEdat.csv", stringsAsFactors = FALSE)
NEdat$cofips <- sprintf("%03d", NEdat$cofips)
NEdat <- left_join(NEdat, HC[,c("cofips", "County")])
NEdat <- mutate(NEdat, yrmo = paste(year, month))
NEdat$yrmo <- parse_date_time(NEdat$yrmo, "ym")
NEdat <- NEdat[NEdat$yrmo <= "2018-02-01",]

#summarize by month for making lags
NEdat <- NEdat %>%
  group_by(County, year, month) %>%
  summarize(tmean = mean(temp), ppt = sum(ppt))

#standardize temperature 
NEdat <- NEdat %>% group_by(County, month) %>% 
  mutate(sdtmean = (tmean - mean(tmean)) /sd(tmean))

#standardize precipitation
NEdat <- NEdat %>% group_by(County, month) %>% 
  mutate(sdppt = (ppt - mean(ppt)) / sd(ppt))

NEdat <- as.data.frame(NEdat)

# West Wide Drought Tracker data

# spi

spi <- read.csv("data/NEco_spi1_2019-12-15.csv", stringsAsFactors = FALSE)
spi <- spi %>%
  mutate(YrMo=paste(year, month, sep="-"))
spi$YrMo <- parse_date_time(spi$YrMo, "ym")
spi$YrMo <- as.Date(spi$YrMo)
spi <- spi[spi$YrMo <= "2018-02-01",]
spi <- rename(spi, spi = spi1)
spi$County <- trimws(spi$County)
spi$County <- gsub(" ", "", spi$County)
# spi <- spi[,c(2:3,6:7)]

#spei

spei <- read.csv("data/NEco_spei1_2019-12-15.csv", stringsAsFactors = FALSE)
spei <- spei %>%
  mutate(YrMo=paste(year, month, sep="-"))
spei$YrMo <- parse_date_time(spei$YrMo, "ym")
spei$YrMo <- as.Date(spei$YrMo)
spei <- spei[spi$YrMo <= "2018-02-01",]
spei <- rename(spei, spei = spei1)
spei$County <- trimws(spei$County)
spei$County <- gsub(" ", "", spei$County)

#load lag maker

source("code/teller_2016_make_data.R")

#Fully automated lag-looping 

#enter the number of months of lags that you'd like to make
lagLengths <- c(12, 18, 24, 30, 36) 
#enter the number of different lags you are making
listOfLagLengths <- vector("list", length = 5)
names(listOfLagLengths) <- lagLengths

#enter the number of variables you are lagging
listofVars <- vector("list", length=4) #as length
names(listofVars) <- 1:4 #and as number of list items

lagnames <- unique(HC$County)
listOfLags <- vector("list", length=length(lagnames))
names(listOfLags) <- lagnames


for (j in seq_along(listOfLagLengths)) {
  nUnits <- lagLengths[[j]]
  
  #spi lags
  for (i in seq_along(lagnames)) {
    County.i <- lagnames[[i]]
    spi.i <- filter(spi, County == County.i)
    dry.i <- select(spi.i, year, month, spi) 
    HC.i <- filter(HC, County == County.i)
    HC.i <- select(HC.i, County, year, cases, Lcases, CI, pop100K) 
    lags.i <- makeDat(dry.i, HC.i, 2, "spi") #the "2" means "February. Pick your start month.
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
    spei.i <- filter(spei, County == County.i)
    dry.i <- select(spei.i, year, month, spei) 
    HC.i <- filter(HC, County == County.i)
    HC.i <- select(HC.i, County, year, cases, Lcases, CI, pop100K) 
    lags.i <- makeDat(dry.i, HC.i, 2, "spei") #the "2" means "February. Pick your start month.
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
    x.i <- filter(NEdat, County == County.i)
    ppt.i <- select(x.i, year, month, sdppt) 
    HC.i <- filter(HC, County == County.i)
    HC.i <- select(HC.i, County, year, cases, Lcases, CI, pop100K) 
    lags.i <- makeDat(x.i, HC.i, 2, "sdppt") #the "2" means "February. Pick your start month.
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
    x.i <- filter(NEdat, County == County.i)
    tmean.i <- select(x.i, year, month, sdtmean) 
    HC.i <- filter(HC, County == County.i)
    HC.i <- select(HC.i, County, year, cases, Lcases, CI, pop100K) 
    lags.i <- makeDat(x.i, HC.i, 2, "sdtmean") #the "2" means "February. Pick your start month.
    lags.i$County <- County.i 
    listOfLags[[i]] <- lags.i 
  }
  
  rows <- do.call(rbind, listOfLags)
  names(rows)[names(rows)== "envCovar"] <- paste0("tmean",nUnits)
  names(rows)[names(rows)=="lags"] <- paste0("lags_tmean",nUnits)
  
  listofVars[[4]] <- rows
  
  merge.all <- function(x, y) {
    merge(x, y, all=TRUE, by=c("County", "year", "cases", "Lcases", "CI", "pop100K"))
  }
  
  listOfLagLengths[[j]] <- Reduce(merge.all, listofVars)
  
}

allLags <- Reduce(merge.all, listOfLagLengths)


