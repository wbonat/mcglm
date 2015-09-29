#' Calculate Variance-Covariance matrix for a fitted McGLM object.
#'
#' @description Returns the variance-covariance matrix for all parameters of a mcglm fitted model object.
#'
#' @param object a fitted model mcglm object.
#' @return A variance-covariance matrix.
#' @export

vcov.mcglm <- function(object) {
    cod <- coef(object)$Parameters
    colnames(object$vcov) <- cod
    rownames(object$vcov) <- cod
    return(object$vcov)
} 
