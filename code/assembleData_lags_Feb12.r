# assemble data through latest year (2012)

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(readxl)

#assemble Human Cases data

oldpops <- read.csv("olddata/NE_County_Pops_2017.csv", stringsAsFactors = FALSE)
oldpops <- oldpops[,1:11]
pop <- read.csv("data/census/PEP_2017_PEPANNRES_cleaner.csv", stringsAsFactors = FALSE)
pop <- pop[grepl("Nebraska", pop$Geography),]
pop$X2018 <- pop$X2017 # update population when it's out
pop$County <- gsub(" County, Nebraska", "", pop$Geography)
pop$County <- gsub(" ", "", pop$County)
pop$cofips <- substr(pop$Id2, start = 3, stop = 5)

pop <- pop[,4:14]
pop <- merge(oldpops, pop, by = "County")

pop <- gather(pop, key = year, value = pop, 2:20)
pop$year <- gsub("^X", "", pop$year)

non <- read_excel("data/Arbonet/NonNeuroCounty_WNV.xlsx")
neuro <- read_excel("data/Arbonet/NeuroCounty_WNV.xlsx")
neuro <- rename(neuro, ncount = COUNT)

all <- full_join(non, neuro)
all <- all[all$State == "NE",]
all[is.na(all)] <- 0
all <- mutate(all, cases = ncount + COUNT)
all$County <- gsub("County", "", all$County)
all$County <- gsub(" ", "", all$County)
# all$cofips <- substr(all$fipscode, start = 3, stop = 5)
cases <- all[,c("County", "Year", "cases")]
cases <- rename(cases, year = Year)

HC18 <- read.csv("data/Arbonet/prelim_2018_HC.csv", stringsAsFactors = FALSE)
HC18$year <- as.character(HC18$year)
cases <- bind_rows(cases, HC18) 

###

#tot2012 <- cases %>% filter(year == 2012) %>% summarize(cases2012 = sum(cases)) #193

#tot2003 <- cases %>% filter(year == 2003) %>% summarize(cases2003 = sum(cases)) #1942

###

HCcases <- left_join(pop, cases, by = c("County", "year"))

HCcases[is.na(HCcases)] <- 0

#Compute cumulative indicence

HCcases <- mutate(HCcases, pop100K=pop/100000)
HC <- HCcases %>%
  group_by(County) %>%
    mutate(CI = lag(cumsum(cases/pop100K)),
           Lcases = lag(cases))
HC$year <- as.integer(HC$year)

HC <- HC[HC$year <= 2012,]

# HC <- ungroup(HC)
# 
# tot2012 <- HC %>% filter(year == 2012) %>% summarize(cases2012 = sum(cases)) #193
# 
# tot2003 <- HC %>% filter(year == 2003) %>% summarize(cases2003 = sum(cases)) #1942

# ppt and temp

NEdat <- read.csv("data/NEdat.csv", stringsAsFactors = FALSE)
NEdat$cofips <- sprintf("%03d", NEdat$cofips)
NEdat <- left_join(NEdat, HC[,c("cofips", "County")])
NEdat <- mutate(NEdat, yrmo = paste(year, month))
NEdat$yrmo <- parse_date_time(NEdat$yrmo, "ym")
NEdat <- NEdat[NEdat$yrmo <= "2012-02-01",]

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


# West Wide Drought Tracker data, processed via data/open_ncdf_indi.r

# spi

spi <- read.csv("data/NEco_spi1_2019-12-15.csv", stringsAsFactors = FALSE)
spi <- spi %>%
  mutate(YrMo=paste(year, month, sep="-"))
spi$YrMo <- parse_date_time(spi$YrMo, "ym")
spi$YrMo <- as.Date(spi$YrMo)
spi <- spi[spi$YrMo <= "2012-02-01",]
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
spei <- spei[spi$YrMo <= "2012-02-01",]
spei <- rename(spei, spei = spei1)
spei$County <- trimws(spei$County)
spei$County <- gsub(" ", "", spei$County)

#load lag maker

source("teller_2016_make_data.R")


#Fully automated lag-looping 

lagLengths <- c(12, 18, 24, 30, 36)
listOfLagLengths <- vector("list", length = 5)
names(listOfLagLengths) <- lagLengths

listofVars <- vector("list", length=4)
names(listofVars) <- 1:4

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
    lags.i <- makeDat(dry.i, HC.i, 2, "spi")
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
    lags.i <- makeDat(dry.i, HC.i, 2, "spei")
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
    lags.i <- makeDat(x.i, HC.i, 2, "sdppt")
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
    lags.i <- makeDat(x.i, HC.i, 2, "sdtmean")
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


# tot2012 <- allLags %>% filter(year == 2012) %>% summarize(cases2012 = sum(cases)) #193
# 
# tot2003 <- allLags %>% filter(year == 2003) %>% summarize(cases2003 = sum(cases)) #1942
