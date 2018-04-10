#' @title Measures of Goodness-of-Fit
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract the pseudo Gaussian log-likelihood (plogLik),
#' pseudo Akaike Information Criterion (pAIC), pseudo Kullback-Leibler
#' Information Criterion (pKLIC) and pseudo Bayesian Information Criterion (pBIC)
#' for objects of \code{mcglm} class.
#'
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @return Returns a data frame containing goodness-of-fit measures.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @source Wang, M. (2014). Generalized Estimating Equations in Longitudinal Data
#' Analysis: A Review and Recent Developments. Advances in Statistics, 1(1)1--13.
#'
#' @seealso \code{plogLik}, \code{pAIC}, \code{pKLIC} and \code{pBIC}.
#' @export

gof <- function(object) {
  pl <- plogLik(object, verbose = FALSE)
  AIC <- pAIC(object, verbose = FALSE)
  KLIC <- pKLIC(object, verbose = FALSE)
  BIC <- pBIC(object, verbose = FALSE)
  output <- data.frame("plogLik" = pl$plogLik, "Df" = pl$df,
                       "pAIC" = AIC$pAIC,"pKLIC" = KLIC$pKLIC,
                       "BIC" = BIC)
  return(output)
}
