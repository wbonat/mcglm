#' @title Measures of Goodness-of-Fit
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract the pseudo Gaussian log-likelihood (plogLik),
#' pseudo Akaike Information Criterion (pAIC), pseudo Kullback-Leibler
#' Information Criterion (pKLIC) and Error Sum of Squares (ESS)
#' for objects of \code{mcglm} class.
#'
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @return Returns a data frame containing goodness-of-fit measures.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @source Wang, M. (2014). Generalized Estimating Equations in Longitudinal Data
#' Analysis: A Review and Recent Developments. Advances in Statistics, 1(1)1--13.
#'
#' @seealso \code{plogLik}, \code{pAIC}, \code{pKLIC}, \code{ESS}.
#' @export

gof <- function(object) {
  pl <- plogLik(object, verbose = FALSE)
  AIC <- pAIC(object, verbose = FALSE)
  KLIC <- pKLIC(object, verbose = FALSE)
  ESS2 <- ESS(object, verbose = FALSE)
  output <- data.frame("plogLik" = pl$plogLik, "Df" = pl$df,
                       "pAIC" = AIC$pAIC,"pKLIC" = KLIC$pKLIC,
                       "ESS" = ESS2)
  return(output)
}
