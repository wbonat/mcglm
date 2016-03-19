#' @title Extract pseudo Akaike Information Criterion (pAIC) for
#' multivariate covariance generalized linear models.
#' @author Wagner Hugo Bonat
#'
#' @description Extract the pAIC for a fitted McGLM.
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param verbose Logical
#' @return Returns the value of the pAIC.
#' @export

pAIC <- function(object, verbose = TRUE) {
  Pseudo <- plogLik(object = object, verbose = FALSE)
  pAIC <- -2*Pseudo$plogLik + 2*Pseudo$df
  if (verbose) cat("pAIC", pAIC)
  return(invisible(list("pAIC" = pAIC)))
}
