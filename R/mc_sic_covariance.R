#' Compute the score information criterion (SIC) for multivariate
#' covariance generalized linear models.
#'
#' @description Compute SIC for covariance parameters in McGLMS.
#' @param object an object representing a model of \code{mcglm} class.
#' @param scope a list of matrices to be tested in the matrix linear
#' predictor.
#' @param data data frame containing all variables envolved in the model.
#' @param penalty penalty term (default = 2).
#' @param response Indicate for which response variable SIC is computed.
#' @return A data frame with SIC values for each matrix in the scope
#' argument.
#' @export

mc_sic_covariance <- function(object, scope, data, penalty = 2,
                              response) {
  for (j in 1:length(scope)) {
  tau <- coef(object, type = "tau",
                       response = response)$Estimates
  n_tau <- length(tau)
  list_tau_new <- list(c(tau, 0))
  n_tau_new <- n_tau + 1
  if(object$power_fixed[[response]]){
    list_power <- object$list_initial$power
  } else {
      list_power <- list(coef(object, type = "power",
                         response = response)$Estimates)
      n_tau_new <- n_tau_new + 1
  }
  list_Z_new <- list(c(object$matrix_pred[[response]], scope[[j]]))
  if(length(object$mu_list) == 1){rho = 0} else {
    rho = coef(object,type = "correlation")$Estimates
  }
  Cfeatures <- mc_build_C(list_mu = object$mu_list,
                          list_Ntrial = object$Ntrial,
                          rho = rho, list_tau = list_tau_new,
                          list_power = list_power,
                          list_Z = list_Z_new,
                          list_sparse = object$sparse,
                          list_variance = object$variance,
                          list_covariance = object$covariance,
                          list_power_fixed = object$power_fixed,
                          compute_C = TRUE)
  temp_score <- mc_pearson(y_vec = object$observed,
                           mu_vec = object$mu_list[[response]]$mu,
                           Cfeatures = Cfeatures, correct = FALSE,
                           compute_variability = TRUE)

  J <- temp_score$Sensitivity
  Sigma <- temp_score$Variability
  Sigma22 <- Sigma[n_tau_new,n_tau_new]
  J21 <- J[n_tau_new, 1:n_tau]
  J11 <- solve(J[1:n_tau,1:n_tau])
  Sigma12 <- Sigma[1:n_tau, n_tau_new]
  Sigma21 <- Sigma[n_tau_new, 1:n_tau]
  J12 <- J[1:n_tau,n_tau_new]
  Sigma11 <- Sigma[1:n_tau,1:n_tau]

  V2 <- Sigma22 - J21%*%J11%*%Sigma12 - Sigma21%*%J11%*%J12 +
    J21%*%J11%*%Sigma11%*%J11%*%J12
  Tu <- t(temp_score$Score[n_tau_new]%*%solve(V2)%*%temp_score$Score[n_tau_new])
  sic[j] <- sqrt(as.numeric(Tu)) - 2
  }
  return(data.frame("SIC" = sic, "Df" = rep(1,length(sic))))
}

