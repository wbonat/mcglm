#' @title Pseudo Akaike Information Criterion
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract the pseudo Akaike information criterion (pAIC)
#' for objects of \code{mcglm} class.
#'
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param verbose logical. Print or not the pAIC value.
#' @return Returns the value of the pseudo Akaike information criterion (pAIC).
#'
#' @seealso \code{gof}, \code{plogLik}, \code{ESS}, \code{pKLIC},
#' \code{GOSHO} and \code{RJC}.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @export

pAIC <- function(object, verbose = TRUE) {
  Pseudo <- plogLik(object = object, verbose = FALSE)
  pAIC <- 2*Pseudo$df - 2*Pseudo$plogLik
  if (verbose) cat("pAIC", pAIC)
  return(invisible(list("pAIC" = pAIC)))
}
