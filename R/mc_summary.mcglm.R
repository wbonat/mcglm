#' Summarizing Multivariate Covariance Generalized Linear Models fits.
#'
#' @description Summary for mcglm objects.
#'
#' @param object an object of class mcglm, usually, a result of a call to \code{mcglm}.
#' @exportMethod

summary.mcglm <- function(object) {
  n_resp <- length(object$beta_names)
  output <- list()
  for(i in 1:n_resp) {
    cat("Call: ")
    print(object$linear_pred[[i]])
    cat("\n")
    cat("Link function:", object$link[[i]])
    cat("\n")
    cat("Variance function:", object$variance[[i]])
    cat("\n")
    cat("Covariance function:", object$covariance[[i]])
    cat("\n")
    cat("Regression:\n")
    tab_beta <- coef(object, std.error = TRUE, response = i, type = "beta")[,1:2]
    tab_beta$"z value" <- tab_beta[,1]/tab_beta[,2]
    rownames(tab_beta) <- object$beta_names[[i]]
    output[i][[1]]$Regression <- tab_beta
    print(tab_beta)
    cat("\n")
    cat("Power:\n")
    tab_power <- coef(object, std.error = TRUE, response = i, type = "power")[,1:2]
    tab_power$'z value' <- tab_power[,1]/tab_power[,2]
    rownames(tab_power) <- NULL
    output[i][[1]]$Power <- tab_power
    print(tab_power)
    cat("\n")
    cat("tau:\n")
    tab_tau <- coef(object, std.error = TRUE, response = i, type = "tau")[,1:2]
    tab_tau$'z value' <- tab_tau[,1]/tab_tau[,2]
    rownames(tab_tau) <- NULL
    output[i][[1]]$tau <- tab_tau
    print(tab_tau)
    cat("\n")
  }
  cat("Correlation matrix:\n")
  tab_rho <- coef(object, std.error = TRUE, response = NA, type = "correlation")[,c(3,1,2)]
  tab_rho$'z value' <- tab_rho[,2]/tab_rho[,3]
  print(tab_rho)
  cat("\n")
  cat("Algorithm:")
  print(object$con$method)
  cat("Correction:")
  print(object$con$correct)
  cat("Number iterations:")
  iteration_cov <- length(na.exclude(object$IterationCovariance[,1]))
  print(iteration_cov)
}

