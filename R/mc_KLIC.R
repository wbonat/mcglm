#' @title Extract pseudo Kullback-Leibler Information Criterion for
#' multivariate covariance generalized linear models.
#' @author Wagner Hugo Bonat
#'
#' @description Extract the pKLIC for a fitted McGLM.
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param verbose Logical
#' @return Returns the value of the pKLIC.
#' @export

pKLIC <- function(object, verbose = TRUE) {
  if(class(object) ==  "mcglm") {
    Pseudo <- plogLik(object = object, verbose = FALSE)
    penalty <- -sum(diag(object$joint_inv_sensitivity%*%object$joint_variability))
    pKLIC <- -2*Pseudo$plogLik + 2*penalty
    if (verbose) cat("pKLIC", pKLIC)
    return(invisible(list("pKLIC" = pKLIC)))
  }
  if(class(object) == "list") {
    Pseudo <- plogLik(object = object, verbose = FALSE)
    jis <- bdiag(lapply(object, function(x)x$joint_inv_sensitivity))
    jv <- bdiag(lapply(object, function(x)x$joint_variability))
    penalty <- -sum(diag(jis%*%jv))
    pKLIC <- -2*Pseudo$plogLik + 2*penalty
    if (verbose) cat("pKLIC", pKLIC)
    return(invisible(list("pKLIC" = pKLIC)))
  }
}

