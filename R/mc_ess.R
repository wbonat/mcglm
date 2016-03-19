#' @title Extract generalized error sum of squares (ESS) for multivariate
#'     covariance generalized linear models.
#' @author Wagner Hugo Bonat
#'
#' @description Extract the generalized error sum of squares for a fitted
#' McGLM.
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param verbose Logical
#' @return Returns the value of the Gaussian pseudo-loglikelihood.
#' @export

ESS <- function(object, verbose = TRUE) {
  if(class(object) == "mcglm") {
    b <- c(as.matrix(object$observed)) - c(as.matrix(object$fitted))
    ess <- as.numeric(t(b)%*%object$inv_C%*%b)
    df <- length(coef(object)$Estimates)
    ess <- ess/df
    if (verbose) cat("ESS", ess)
    return(invisible(list("ESS" = ess)))
  }
  if(class(object) == "list") {
    Y <- do.call(c,lapply(object, function(x)as.numeric(x$observed)))
    mu <-do.call(c,lapply(object, function(x)as.numeric(x$fitted)))
    b <- Y - mu
    C.list <- lapply(object, function(x)x$C)
    inv_C.list <- lapply(object, function(x)x$inv_C)
    inv_C <- bdiag(inv_C.list)
    ess <- as.numeric(t(b)%*%inv_C%*%b)
    df <- sum(unlist(lapply(object, function(x)length(coef(x)$Estimates))))
    ess <- ess/df
    if (verbose) cat("ESS", ess)
    return(invisible(list("ESS" = ess)))
  }
}
