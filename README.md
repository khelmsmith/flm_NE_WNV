# flm_NE_WNV

Testing pull request workflow.
Seems to work. 

## Functional linear modeling of West Nile Virus in Nebraska

Simulated case data and actual climate and population covariates used in 
Helm-Smith, K., et al. (in review) Using climate to explain and predict West Nile Virus risk in Nebraska.
Geohealth manuscript 2020GH000244.

[make a pdf preprint with that citation on every page and put in root folder of repository?]

## Data

NE_county_pops.csv is Nebraska County populations, 2000-2018, from the U.S. Census Bureau, using annual estimates

sampledat.csv is simulated data on annual numbers of human cases of neuro-invasive and non-neuro-invasive West Nile Virus in Nebraska counties. It is predictions of a model that was trained on actual numbers of cases as recorded in CDC's Arbonet database. It excludes Arthur County, because no cases have been recorded there to date, and we had to exclude it from our modeling to get it to work. 

NEdat.csv includes temperature and precipitation data for Nebraska counties each month from January 1998 to February 2019 from the National Centers for Environmental Information, National Climatic Data Center (ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/).

NEco_spei1_2019-12-15.csv and NEco_spi1_2019-12-15.csv are extracted monthly values from Westwide Drought Tracker netcdf files. See Abatzoglou, J. T., McEvoy, D. J., & Redmond, K. T. (2017). The West Wide Drought Tracker: Drought monitoring at fine spatial scales. Bulletin of the American Meteorological Society, 98(9), 1815–1820. https://doi.org/10.1175/BAMS-D-16-0193.1

## Code

*WNV_models_lags.r* examines combinations of variables and lag lengths. 
Output includes degrees of freedom, log likelihood, Akaike's Information Criterion, Bayes Information Criterion, deviance, and df.residuals for each model fomula. It's written to save results to a "temp" folder. 

*predict_wYr.r* models a county-year variable to get predictions for both training data and out-of-sample data for a model that incorporates the year contrast coefficient. Change lines 29 and 50 according to the model you are using. Output is called "allIndepData"", with County, year, cases, Lcases (lagged cases, for comparing predictions with a simple naive model), fit, standard error, and prediction. 

*predict_noYr.r* is to get predictions from a model that does not use the year contrast coefficient. Change line 29 according to the model you are using.

*see_lags.R* calls `extract_functional.r` to extract the coefficients for the functional smoothing curves and then ggplots them. 

*assembleData_lags_Feb18.r* brings together data on population, human cases, temperature, precipitation, and Standardized Precipitation (and Evapotranspiration) indices, and generates lags of high-density predictors. Note that this is based on "what would we have known in Feb. 2018?" so data is trimmed to Feb. 2018. (Incorporating more data, such as from later months of the year, changes mean temperature and precipitation calculations.) You can change the number of months of lags, the number of lags, and the starting month of lags. Places to do this are indicated in comments in the code. Output is a dataframe called "allLags".

*teller_2016_make_data.R* creates lags of variables and is sourced into the `assembleData_lags_Feb18.r`. It is adapted from supplemental material published with Teller, B. J., Adler, P. B., Edwards, C. B., Hooker, G., & Ellner, S. P. (2016). Linking demography with drivers: Climate and competition. Methods in Ecology and Evolution, 7(2), 171–183. https://doi.org/10.1111/2041-210X.12486