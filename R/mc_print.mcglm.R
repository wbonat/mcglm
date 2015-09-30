#' Print method for Multivariate Covariance Generalized Linear Model
#'
#' @description The default print method for a mcglm object.
#' @method print mcglm
#' @param x fitted model objects of class mcglm as produced by mcglm().
#' @param ... further arguments passed to or from other methods.
#' @export
#' @rdname print.mcglm

print.mcglm <- function(x, ...) {
    object <- x
    n_resp <- length(object$beta_names)
    regression <- mc_updateBeta(list_initial = list(), betas = object$Regression, information = object$Information, n_resp = n_resp)
    for (i in 1:n_resp) {
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
        cat("Dispersion:\n")
        tau_temp <- coef(object, response = i, type = "tau")$Estimate
        names(tau_temp) <- rep("", length(tau_temp))
        print(tau_temp)
        cat("\n")
        power_temp <- coef(object, response = i, type = "power")$Estimate
        if (length(power_temp) != 0) {
            names(power_temp) <- ""
            cat("Power:\n")
            print(power_temp)
            cat("\n")
        }
    }
}
