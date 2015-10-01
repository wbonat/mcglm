#' Compute the score information criterion (SIC) for multivariate
#' covariance generalized linear models.
#'
#' @description Compute the SIC for McGLMS.
#' @param object an object representing a model of \code{mcglm} class.
#' @param scope a vector containing all covariate names to be tested.
#' @param data data frame containing the all variables envolved
#' @param penalty penalty term (default = 2).
#' @param response Indicate for which response variable SIC is computed.
#' @return A data frame with SIC values for each covariate in the scope
#' argument.
#' @export

mc_sic <- function (object, scope, data, response, penalty = 2) {
  SIC <- c()
  df <- c()
  for(i in 1:length(scope)){
  ini_formula <- object$linear_pred[[response]]
  ext_formula <- as.formula(paste("~", paste(ini_formula[3],
                                             scope[i], sep = "+")))
  md <- model.frame(object$linear_pred[[response]], data = data)
  Y = model.response(md)
  ini_beta <- coef(object, type = "beta", response = response)$Estimates
  ext_X <- model.matrix(ext_formula, data = data)
  n_beta <- dim(ext_X)[2]
  n_ini_beta <- length(ini_beta)
  ext_beta <- c(ini_beta, rep(0, n_beta - n_ini_beta))
  n_total_beta <- length(ext_beta)
  mu_temp <- mc_link_function(beta = ext_beta, X = ext_X, offset = NULL,
                              link = object$link[[response]])
  score_temp <- mc_quasi_score(D = mu_temp$D, inv_C = object$inv_C,
                               y_vec = Y, mu_vec = mu_temp$mu)
  S11 <- score_temp$Variability[1:n_ini_beta,1:n_ini_beta]
  S22 <- score_temp$Variability[c(n_ini_beta+1):n_total_beta,
                                c(n_ini_beta+1):n_total_beta]
  S12 <- score_temp$Variability[1:n_ini_beta,
                                c(n_ini_beta+1):n_total_beta]
  S21 <- score_temp$Variability[c(n_ini_beta+1):n_total_beta,
                                1:n_ini_beta]
  VB <- S22 - S21 %*% solve(S11) %*% S12
  Tu <- t(score_temp$Score[c(n_ini_beta+1):n_total_beta])%*%
    solve(VB)%*%score_temp$Score[c(n_ini_beta+1):n_total_beta]
  df[i] <- n_beta - n_ini_beta
  SIC[i] <- as.numeric(sqrt(Tu)) - penalty*df[i]
  }
  output <- data.frame("SIC" = SIC, "Covariance" = scope, "df" = df)
  return(output)
}

