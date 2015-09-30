#' Calculate Variance-Covariance matrix for a fitted McGLM object.
#'
#' @description Returns the variance-covariance matrix for all parameters of a mcglm fitted model object.
#'
#' @param object a fitted model mcglm object.
#' @param ... additional arguments affecting the summary produced. Note that there is no extra options for
#' mcglm object class.
#' @return A variance-covariance matrix.
#' @export

vcov.mcglm <- function(object, ...) {
    cod <- coef(object)$Parameters
    colnames(object$vcov) <- cod
    rownames(object$vcov) <- cod
    return(object$vcov)
}
