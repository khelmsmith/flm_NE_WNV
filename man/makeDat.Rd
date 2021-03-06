% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teller_2016_make_data.R
\name{makeDat}
\alias{makeDat}
\title{Assemble data with lagged environmental predictors}
\usage{
makeDat(envData, response, monthStart, meas, nUnits)
}
\arguments{
\item{envData}{values of environmental data, in this case, by month and county}

\item{response}{the response variable, human cases of WNV}

\item{monthStart}{the month from which to lag the environmental data back}

\item{meas}{month}

\item{nUnits}{how many units (meas) to go back}
}
\value{
a matrix of lagged environmental variables
}
\description{
Assemble data with lagged environmental predictors
}
