# First, you can find the data for the various metrics here under *_REGIONS_PRISM.nc
# https://wrcc.dri.edu/wwdt/data/REGION/

#library(RCurl)
#library(httr)
library(ncdf4)
library(raster)

options(scipen = 100)

# lookup.csv created by wrangle_lookup.r -- shouldn't need to be changed
lookup <- readr::read_csv("data-raw/lookup.csv")
#url <- "https://www.wrcc.dri.edu/wwdt/data/PRISM/"
#"http://www.wrcc.dri.edu/wwdt/data/PRISM/spei1/spei1_1_PRISM.nc"

# download using wget and a batch file ... couldn't get httr to work
shell(file.path(getwd(),"data-raw","wwdt_wget.bat"))

# what indicators do you want?
indi <- c("spei1","spi1")

filestem <- "_REGIONS_PRISM.nc"
for (i in indi){
    # tmp = tempfile()
    # f <- CFILE(tmp,"wb")
    # curlPerform(url = paste0(url,i,"/",i,filestem),
    #             writedata = f@ref)
    # close(f)
    tmp <- paste0("data-raw/",i,filestem)
    nc = nc_open(tmp)
    DATE <- ncvar_get(nc, "date")
    DATE <- as.Date(DATE, origin = "1900-01-01")
    thru <- max(DATE)
    # bizarrely, data matrix only has missing values for the *last* month of the current year
    # and 0.0 for values between last month and November ...
    # pick based on Date, also discard columns prior to 1997 ...
    gooddata <- DATE < lubridate::floor_date(Sys.Date(), "month") &
      DATE > lubridate::ymd("1997-01-01")
    # data matrix, locations are rows, time is columns
    dat <- ncvar_get(nc, "data", raw_datavals = TRUE)
    # column of location polygon ids
    polygon <- ncvar_get(nc, attributes(nc$dim)$names[1])
    polygon <- as.vector(polygon) # convert to numeric vector
    ddat <- as.data.frame(dat[,gooddata]) # strip off missing months and prior to 1997
    colnames(ddat) <- DATE[gooddata]
    ddat <- cbind(polygon, ddat) # finally! a usuable object!
    ddat <- dplyr::inner_join(lookup, ddat) # adds in the county and state names, plus a type variable
    ddat <- dplyr::filter(ddat, type == "county") # remove excess rows for speed
    ddat <- tidyr::separate(ddat, name, into = c("state", "county"), sep = " - ")
    ddat <- tidyr::pivot_longer(ddat, -(1:4), names_to = "date", values_to = i)
    ddat$date <- lubridate::ymd(ddat$date)
    ddat$year <- lubridate::year(ddat$date)
    ddat$month <- lubridate::month(ddat$date)
    ddat$county <- gsub("County", "", ddat$county) # toggle this line on or off to keep "County" in the name
    ddat$county <- stringi::stri_trim(ddat$county) # toggle this line on or off to keep "County" in the name
    ddat$polygon <- as.character(ddat$polygon)
    ddat <- dplyr::select(ddat, -type)
    assign(i,ddat) # name the object after the indicator picked
    nc_close(nc)
    file.remove(tmp) # clean up
}
save(list = indi, file = "data/wwdt.rda")




