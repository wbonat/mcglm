#' @title Pseudo Bayesian Information Criterion
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract the pseudo Bayesian information criterion (pBIC)
#' for objects of \code{mcglm} class.
#'
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param verbose logical. Print or not the pBIC value.
#' @return Returns the value of the pseudo Bayesian information criterion (pBIC).
#'
#' @seealso \code{gof}, \code{plogLik}, \code{ESS}, \code{pKLIC},
#' \code{GOSHO} and \code{RJC}.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @export

pBIC <- function(object, verbose = TRUE) {
  Pseudo <- plogLik(object = object, verbose = FALSE)
  pBIC <- Pseudo$df*log(length(object$observed)) - 2*Pseudo$plogLik
  if (verbose) cat("pBIC", pBIC)
  return(invisible(list("pBIC" = pBIC)))
}
