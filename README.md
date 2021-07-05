# flm.NE.WNV

## Functional linear modeling of West Nile Virus in Nebraska

Simulated case data and actual climate and population covariates used in 
Smith, K.H., et al. (in review) Using climate to explain and predict West Nile Virus risk in Nebraska.
Geohealth manuscript 2020GH000244.

[make a pdf preprint with that citation on every page and put in root folder of repository?]

## To install the package

```r
devtools::install_github("khelmsmith/flm_NE_WNV")
```

For how to run it, see NE_WNV.rmd in "Vignettes" on the repository. If you want
to have the vignette locally, use 

```r
devtools::install_github("khelmsmith/flm_WNV_NE", build_vignettes = TRUE)
```

but keep in mind that this takes about 45 minutes on a modern laptop.

## Data Sources

NE_county_pops.csv is Nebraska County populations, 2000-2018, from the U.S. Census Bureau, using annual estimates

sampledat.csv is simulated data on annual numbers of human cases of neuro-invasive and non-neuro-invasive West Nile Virus in Nebraska counties. It is predictions of a model that was trained on actual numbers of cases as recorded in CDC's Arbonet database. It excludes Arthur County, because no cases have been recorded there to date, and we had to exclude it from our modeling to get it to work. 

NEdat.csv includes temperature and precipitation data for Nebraska counties each month from January 1998 to February 2019 from the National Centers for Environmental Information, National Climatic Data Center (ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/).

NEco_spei1_2019-12-15.csv and NEco_spi1_2019-12-15.csv are extracted monthly values from Westwide Drought Tracker netcdf files. See Abatzoglou, J. T., McEvoy, D. J., & Redmond, K. T. (2017). The West Wide Drought Tracker: Drought monitoring at fine spatial scales. Bulletin of the American Meteorological Society, 98(9), 1815–1820. https://doi.org/10.1175/BAMS-D-16-0193.1

## Code

*assemble.data.lags* brings together data on population, human cases, temperature, precipitation, and Standardized Precipitation (and Evapotranspiration) indices, and generates lags of predictors. Note that this is based on "what would we have known in Feb. 2018?" so data is trimmed to Feb. 2018. Output is a dataframe called "allLagsT", with training data, 2002-2017, and a dataframe called "allLagsO", with out-of-sample data, 2018. It is sourced in flm. 

*makeDat* creates lags of variables and is part of the data assembly function. It is adapted from supplemental material published with Teller, B. J., Adler, P. B., Edwards, C. B., Hooker, G., & Ellner, S. P. (2016). Linking demography with drivers: Climate and competition. Methods in Ecology and Evolution, 7(2), 171–183. https://doi.org/10.1111/2041-210X.12486

*call.flm* lists all combinations of predictors as model formulae, including lags of precipitation, temperature, SPI and SPEI distributed across different time intervals; cumulative incidence; and contrast coefficients for county and year; fits models; determines which model has the lowest AIC score; and uses the best model to generate out-of-sample predictions.  

*models_lags* is a component of call.flm. It fits a list of models to training data and generates the predictions, including determining whether or not the top model uses the year coefficient, which affects how predictions are generated.

*predict_wYr* models a county-year variable to get predictions for both training data and out-of-sample data for a model that incorporates the year contrast coefficient. Sourced in models_lags. 

*predict_noYr* is to get predictions for both training data and out-of-sample data from a model that does not use the year contrast coefficient. Sourced in models_lags. 

*see.lags* calls `extract_functional` to extract the coefficients for the functional smoothing curves and ggplots them. 
