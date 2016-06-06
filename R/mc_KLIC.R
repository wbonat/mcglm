#' @title Pseudo Kullback-Leibler Information Criterion
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract the pseudo Kullback-Leibler information criterion
#' (pKLIC) for objects of \code{mcglm} class.
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param verbose logical. Print or not the pKLIC value.
#' @return Returns the value of the pseudo Kullback-Leibler information
#' criterion.
#'
#' @seealso \code{gof}, \code{plogLik}, \code{ESS}, \code{pAIC},
#' \code{GOSHO} and \code{RJC}.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
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

