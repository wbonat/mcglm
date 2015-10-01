#' @title Calculate Variance-Covariance matrix for a fitted McGLM
#'     object.
#' @name vcov.mcglm
#'
#' @description Returns the variance-covariance matrix for all
#'     parameters of a \code{mcglm} fitted model object.
#'
#' @param object a fitted model \code{mcglm} object.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#'
#' @return A variance-covariance matrix.
#'
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @method vcov mcglm
#' @export

vcov.mcglm <- function(object, ...) {
    cod <- coef(object)$Parameters
    colnames(object$vcov) <- cod
    rownames(object$vcov) <- cod
    return(object$vcov)
}
