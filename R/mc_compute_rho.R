#' @title Autocorrelation Estimates
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Compute autocorrelation estimates based on a fitted model
#' using the \code{mc_car} structure. The \code{mcglm} approach fits
#' models using a linear covariance structure, but in general in this
#' parametrization for spatial models the parameters have no simple
#' interpretation in terms of spatial autocorrelation.
#' The function \code{mc_compute_rho} computes the autocorrelation
#' based on a fitted model.
#'
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param level the confidence level required.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @seealso \code{mc_car} and \code{mc_conditional_test}.
#'
#' @return Returns estimate, standard error and confidential interval for
#' the spatial autocorrelation parameter.
#'
#' @export

mc_compute_rho <- function(object, level = 0.975) {
  tau <- coef(object, type = "tau")$Estimates[1:2]
  rho <- tau[2]/tau[1]
  derivada <- Matrix(c(-tau[2]/tau[1]^2, 1/tau[1]), 1, 2)
  std <- as.numeric(sqrt(derivada%*%vcov(object)[c("tau11", "tau12"), c("tau11", "tau12")]%*%t(derivada)))
  output <- data.frame("rho" = rho, "std" = std, "Conf.Min" = rho -1*qnorm(level)*std,
                       "Conf.Max" = rho + 1*qnorm(level)*std)
  return(output)
}
