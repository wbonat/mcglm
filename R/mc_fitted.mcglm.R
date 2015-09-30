#' @title Extract Model Fitted Values of McGLM
#' @name fitted.mcglm
#'
#' @description Extract fitted values for objects of \code{mcglm} class.
#'
#' @param object An object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for \code{mcglm} object class.
#'
#' @return Depending on the number of response variable, the function
#'     \code{fitted.mcglm} returns a vector (univariate models) or a
#'     matrix (multivariate models) of fitted values.
#'
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @method fitted mcglm
#' @export

fitted.mcglm <- function(object, ...) {
    n_resp <- length(object$beta_names)
    output <- Matrix(object$fitted, ncol = n_resp, nrow = object$n_obs)
    return(output)
}
