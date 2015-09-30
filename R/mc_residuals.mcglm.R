#' Residuals for Multivariate Covariance Generalized Linear Models (McGLM)
#'
#' @description Compute residuals based on fitting mcglm models.
#'
#' @param object An of class mcglm, typically the result of a call to \code{mcglm}.
#' @param type the type of residuals which should be returned. The alternatives are: 'raw'
#' (default), 'pearson' and 'standardized'.
#' @param ... additional arguments affecting the residuals produced. Note that there is no extra options for
#' mcglm object class.
#' @return Depending on the number of response variable the function \code{residuals.mcglm} returns
#' a vector (univariate models) or a matrix (multivariate models) of residuals values.
#' @export

residuals.mcglm <- function(object, type = "raw", ...) {
    n_resp <- length(object$beta_names)
    output <- Matrix(object$residuals, ncol = n_resp, nrow = object$n_obs)
    if (type == "standardized") {
        output <- Matrix(as.numeric(object$residuals %*% chol(object$inv_C)), ncol = n_resp, nrow = object$n_obs)
    }
    if (type == "pearson") {
        output <- Matrix(as.numeric(object$residuals/sqrt(diag(object$C))), ncol = n_resp, nrow = object$n_obs)
    }
    return(output)
}
