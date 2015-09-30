##' @title Multivariate covariance generalized linear models (McGLMs)
##'
##'
##' @description Fits a multivariate covariance generalized linear models (McGLMs) to data.
##' McGLM is a general framework for non-normal multivariate data analysis, designed to handle
##' multivariate response variables, along with a wide range of temporal and spatial correlation
##' structures defined in terms of a covariance link function combined with a matrix linear predictor
##' involving known matrices. The models take non-normality into account in the conventional way by means
##' of a variance function, and the mean structure is modelled by means of a link function and a linear predictor.
##' The models are fitted using an efficient Newton scoring algorithm based on quasi-likelihood and
##' Pearson estimating functions, using only second-moment assumptions.
##' This provides a unified approach to a wide variety of different types of response variables and
##' covariance structures, including multivariate extensions of repeated measures, time series, longitudinal,
##' spatial and spatio-temporal structures.
##'
##'
##' @docType package
##' @name mcglm
NULL

#' @name ahs
#'
#' @title Australian health survey
#'
#' @description The Australian health survey was used by Bonat and Jorgensen (2015) as an example of multivariate
#' count regression model. The data consists of five count response variables concerning health system access
#' measures and nine covariates concerning  social conditions in Australian for 1987-88.
#'
#' \itemize{
#'     \item \code{sex} - Factor, two levels (0-Male; 1-Female).
#'     \item \code{age} - Respondent's age in years divided by 100.
#'     \item \code{income} - Respondent's annual income in Australian dollars divided by 1000.
#'     \item \code{levyplus} - Factor, two levels (1- if respondent is covered by private health
#'     insurance fund for private patients in public hospital (with doctor of choice); 0 - otherwise).
#'     \item \code{freepoor} - Factor, two levels (1 - if respondent is covered by government because low income,
#'     recent immigrant, unemployed; 0 - otherwise).
#'     \item \code{freerepa} - Factor, two levels (1 - if respondent is covered free by government because of
#'     old-age or disability pension, or because invalid veteran or family of deceased veteran; 0 - otherwise).
#'     \item \code{illnes} - Number of illnesses in past 2 weeks, with 5 or more weeks coded as 5.
#'     \item \code{actdays} - Number of days of reduced activity in the past two weeks due to illness or injury.
#'     \item \code{hscore} - Respondent's general health questionnaire score using Goldberg's method;
#'     high score indicates poor health.
#'     \item \code{chcond1} - Factor, two levels (1 - if respondent has chronic condition(s) but is not limited
#'     in activity; 0 - otherwise).
#'     \item \code{chcond2} - Factor, two levels (1 if respondent has chronic condition(s) and is limited in
#'     activity; 0 - otherwise).
#'     \item \code{Ndoc} - Number of consultations with a doctor or specialist (response variable).
#'     \item \code{Nndoc} - Number of consultations with health professionals (response variable).
#'     \item \code{Nadm} - Number of admissions to a hospital, psychiatric hospital, nursing or
#'     convalescence home in the past 12 months (response variable).
#'     \item \code{Nhosp} - Number of nights in a hospital during the most recent admission.
#'     \item \code{Nmed} - Total number of prescribed and non prescribed medications used in the past two days.
#'     \item \code{id} - Respondent's index.
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(ahs)
#'
#' @format a \code{data.frame} with 5190 records and 17 variables.
#'
#' @source Deb, P. and Trivedi, P. K. (1997). Demand for medical care by the elderly: A finite mixture approach,
#' Journal of Applied Econometrics 12(3):313--336.
#'
NULL
