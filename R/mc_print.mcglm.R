#' Print a Multivariate Covariance Generalized Linear Model
#'
#' @description The default print method for a mcglm object.
#'
#' @param object fitted model objects of class mcglm as produced by mcglm().
#' @export print mcglm
#' @aliases print print.mcglm
print.mcglm <- function(object) {
  n_resp <- length(object$beta_names)
  regression <- mc_updateBeta(list_initial = list(), betas = object$Regression,
                            information = object$Information, n_resp = n_resp)
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
    names(regression[[1]][[i]]) <- object$beta_names[[i]]
    cat("Regression:\n")
    print(regression[[1]][[i]])
    cat("\n")
    cat("tau:\n")
    print(coef(object, response = i, type = "tau")$Estimate)
    cat("\n")
    cat("power:\n")
    print(coef(object, response = i, type = "power")$Estimate)
    cat("\n")
  }
}
