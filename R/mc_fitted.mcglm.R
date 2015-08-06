#' Extract Model Fitted Values of McGLM
#'
#' @description Extract fitted values for objects of mcglm class.
#'
#' @param object An object of mcglm class.
#' @return Depending on the number of response variable the function \code{fitted.mcglm} returns
#' a vector (univariate models) or a matrix (multivariate models) of fitted values.

fitted.mcglm <- function(object){
  n_resp <- length(object$beta_names)
  output <- Matrix(object$fitted, ncol = n_resp, nrow = object$n_obs)
  return(output)
}
