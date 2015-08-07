#' Auxiliar function transforms list to a vector.
#'
#' @description This function takes a list of parameters and tranforms to a vector.
#' @param list_initial A list specifying initial values.
#' @param list_power_fixed A list of logical operators specyfing if the power parameter should be
#' estimated or not.
#' @return A vector of model parameters.
#' @details It is an internal function, in general the users never will use this function.
#' It will be useful, only if the user wants to implement a different variance-covariance matrix.
#' @export
mc_list2vec <- function(list_initial, list_power_fixed) {
  power_vec <- do.call(c, list_initial$power)
  n_power <- do.call(c,lapply(list_initial$power, length))
  indi <- rep(do.call(c, list_power_fixed), n_power)
  power <- data.frame("power" = power_vec, "indi" = indi)
  power_ini <- power[which(power$indi == FALSE),]$power
  beta_ini <- do.call(c,list_initial$regression)
  cov_ini <- c("rho" = list_initial$rho, "power" = power_ini, "tau" = do.call(c,list_initial$tau))
  return(list("beta_ini" = as.numeric(beta_ini), "cov_ini" = as.numeric(cov_ini)))
}
