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
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @export

pBIC <- function(object, verbose = TRUE) {
  Pseudo <- plogLik(object = object, verbose = FALSE)
  if(class(object) == "mcglm") {
    Y = object$observed
  }
  if(class(object) == "list") {
    Y <- do.call(c,lapply(object, function(x)as.numeric(x$observed)))
  }
  NS <- length(Y)
  pBIC <- Pseudo$df*log(NS) - 2*Pseudo$plogLik
  if (verbose) cat("pBIC", pBIC)
  return(invisible(list("pBIC" = pBIC)))
}
