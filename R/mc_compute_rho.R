#' @title Compute autocorrelation estimates
#' @author Wagner Hugo Bonat
#'
#' @description Compute autocorrelation estimates based on models fitting
#' using the \code{mc_car} structure.
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @param level the confidence level required.
#' @return Returns estimate, standard error and confidential interval for
#' the spatial autocorrelation parameter.
#' @export

mc_compute_rho <- function(object, level = 0.975) {
  tau <- coef(object, type = "tau")$Estimates[1:2]
  rho <- tau[2]/tau[1]
  derivada <- Matrix(c(-tau[2]/tau[1]^2, 1/tau[1]), 1, 2)
  std <- as.numeric(sqrt(derivada%*%vcov(object)[c("tau11", "tau12"), c("tau11", "tau12")]%*%t(derivada)))
  output <- data.frame("rho" = rho, "std" = std, "Conf.Min" = rho + -1*qnorm(level)*std,
                       "Conf.Max" = rho + -1*qnorm(level)*std)
  return(output)
}
