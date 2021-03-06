% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mc_core_cross_variability.R
\name{covprod}
\alias{covprod}
\title{Cross variability matrix}
\usage{
covprod(A, res, W)
}
\arguments{
\item{A}{A matrix.}

\item{res}{A vector of residuals.}

\item{W}{A matrix of weights.}
}
\description{
Compute the cross-covariance matrix between covariance
    and regression parameters.  Equation (11) of Bonat and Jorgensen
    (2016).
}
\author{
Wagner Hugo Bonat
}
\keyword{internal}
