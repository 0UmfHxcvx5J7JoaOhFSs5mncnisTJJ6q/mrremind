% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcIEA_WEO_2021.R
\name{calcIEA_WEO_2021}
\alias{calcIEA_WEO_2021}
\title{Calculate REMIND variables from IEA WEO 2021 data}
\usage{
calcIEA_WEO_2021(subtype, isValidation = FALSE)
}
\arguments{
\item{subtype}{Either "global" or "region". On global level, the source offers
more variables than on regional level, but the data should not be used on sub-
global level due to its coarse disaggregation.}

\item{isValidation}{indicates if result will be used in validation (as opposed to generating input data)}
}
\value{
A \code{\link[magclass:magclass-package]{magpie}} object.
}
\description{
Calculate REMIND variables from IEA WEO 2021 data
}
\author{
Falk Benke
}
