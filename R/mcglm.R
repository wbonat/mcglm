##' @title Multivariate covariance generalized linear models (McGLMs)
##'
##' @description Fits a multivariate covariance generalized linear
##'     models (McGLMs) to data.  McGLM is a general framework for
##'     non-normal multivariate data analysis, designed to handle
##'     multivariate response variables, along with a wide range of
##'     temporal and spatial correlation structures defined in terms of
##'     a covariance link function combined with a matrix linear
##'     predictor involving known matrices. The models take
##'     non-normality into account in the conventional way by means of a
##'     variance function, and the mean structure is modelled by means
##'     of a link function and a linear predictor.  The models are
##'     fitted using an efficient Newton scoring algorithm based on
##'     quasi-likelihood and Pearson estimating functions, using only
##'     second-moment assumptions.  This provides a unified approach to
##'     a wide variety of different types of response variables and
##'     covariance structures, including multivariate extensions of
##'     repeated measures, time series, longitudinal, spatial and
##'     spatio-temporal structures.
##'
##' @docType package
##' @name mcglm
NULL

#' @title Australian health survey
#' @name ahs
#'
#' @description The Australian health survey was used by Bonat and
#'     Jorgensen (2015) as an example of multivariate count regression
#'     model. The data consists of five count response variables
#'     concerning health system access measures and nine covariates
#'     concerning social conditions in Australian for 1987-88.
#'
#' \itemize{
#'
#' \item \code{sex} - Factor with levels \code{male} and \code{female}.
#'
#' \item \code{age} - Respondent's age in years divided by 100.
#'
#' \item \code{income} - Respondent's annual income in Australian
#'     dollars divided by 1000.
#'
#' \item \code{levyplus} - Coded factor. If respondent is covered by
#'     private health insurance fund for private patients in public
#'     hospital with doctor of choice (1) or otherwise (0).
#'
#' \item \code{freepoor} - Coded factor. If respondent is covered by
#'     government because low income, recent immigrant, unemployed (1)
#'     or otherwise (0).
#'
#' \item \code{freerepa} - Coded factor. If respondent is covered free
#'     by government because of old-age or disability pension, or
#'     because invalid veteran or family of deceased veteran (1) or
#'     otherwise (0).
#'
#' \item \code{illnes} - Number of illnesses in past 2 weeks, with 5 or
#'     illnesses coded as 5.
#'
#' \item \code{actdays} - Number of days of reduced activity in the past
#'     two weeks due to illness or injury.
#'
#' \item \code{hscore} - Respondent's general health questionnaire score
#'     using Goldberg's method. High score indicates poor health.
#'
#' \item \code{chcond} - Factor with three levels. If respondent has
#'     chronic condition(s) and is limited in activity (\code{limited}),
#'     or if the respondent has chronic condition(s) but is not limited
#'     in activity (\code{nonlimited}) or otherwise (\code{otherwise},
#'     reference level).
#'
#' \item \code{Ndoc} - Number of consultations with a doctor or
#'     specialist (response variable).
#'
#' \item \code{Nndoc} - Number of consultations with health
#'     professionals (response variable).
#'
#' \item \code{Nadm} - Number of admissions to a hospital, psychiatric
#'     hospital, nursing or convalescence home in the past 12 months
#'     (response variable).
#'
#' \item \code{Nhosp} - Number of nights in a hospital during the most
#'     recent admission.
#'
#' \item \code{Nmed} - Total number of prescribed and non prescribed
#'     medications used in the past two days.
#'
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
#' @source Deb, P. and Trivedi, P. K. (1997). Demand for medical care by
#'     the elderly: A finite mixture approach, Journal of Applied
#'     Econometrics 12(3):313--336.
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' data(ahs, package="mcglm")
#' str(ahs)
#'
#' xt <- xtabs(~age+sex, data=ahs)
#' mosaicplot(xt)
#'
#' xt <- xtabs(~age+chcond, data=ahs)
#' mosaicplot(xt)
#'
#' useOuterStrips(
#'     combineLimits(
#'         xyplot(Ndoc+Nndoc+Nadm+Nhosp+Nmed~age|sex,
#'                outer=TRUE, data=ahs,
#'                jitter.x=TRUE, amount=0.01,
#'                type=c("p", "a"),
#'                scales=list(y=list(relation="free")),
#'                ylab="Number or occurences",
#'                xlab="Age (years/100)")
#'     )
#' )
#'
#' useOuterStrips(
#'     combineLimits(
#'         xyplot(Ndoc+Nndoc+Nadm+Nhosp+Nmed~income|sex,
#'                outer=TRUE, data=ahs,
#'                jitter.x=TRUE, amount=0.01,
#'                type=c("p", "a"),
#'                scales=list(y=list(relation="free")),
#'                ylab="Number or occurences",
#'                xlab="Age (years/100)")
#'     )
#' )
#'
NULL
