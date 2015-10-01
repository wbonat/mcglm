#' @title Summarizing Multivariate Covariance Generalized Linear Models
#'     fits.
#' @name summary.mcglm
#'
#' @description Summary for McGLMs objects.
#'
#' @param object an object of class \code{mcglm}, usually, a result of a
#'     call to \code{mcglm}.
#' @param ... additional arguments affecting the summary produced. Note
#'     the there is no extra options for mcglm object class.
#'
#' @return Print an \code{mcglm} object.
#'
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @method summary mcglm
#' @export

summary.mcglm <- function(object, ...) {
    n_resp <- length(object$beta_names)
    output <- list()
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
        cat("Regression:\n")
        tab_beta <- coef(object, std.error = TRUE, response = i, type = "beta")[, 1:2]
        tab_beta$"Z value" <- tab_beta[, 1]/tab_beta[, 2]
        rownames(tab_beta) <- object$beta_names[[i]]
        output[i][[1]]$Regression <- tab_beta
        print(tab_beta)
        cat("\n")
        tab_power <- coef(object, std.error = TRUE, response = i, type = "power")[, 1:2]
        tab_power$"Z value" <- tab_power[, 1]/tab_power[, 2]
        rownames(tab_power) <- NULL
        if (dim(tab_power)[1] != 0) {
            cat("Power:\n")
            print(tab_power)
            output[i][[1]]$Power <- tab_power
            cat("\n")
        }
        cat("Dispersion:\n")
        tab_tau <- coef(object, std.error = TRUE, response = i, type = "tau")[, 1:2]
        tab_tau$"Z value" <- tab_tau[, 1]/tab_tau[, 2]
        rownames(tab_tau) <- NULL
        output[i][[1]]$tau <- tab_tau
        print(tab_tau)
        cat("\n")
    }
    tab_rho <- coef(object, std.error = TRUE, response = NA, type = "correlation")[, c(3, 1, 2)]
    tab_rho$"Z value" <- tab_rho[, 2]/tab_rho[, 3]
    if (dim(tab_rho)[1] != 0) {
        cat("Correlation matrix:\n")
        print(tab_rho)
        cat("\n")
    }
    names(object$con$correct) <- ""
    iteration_cov <- length(na.exclude(object$IterationCovariance[, 1]))
    names(iteration_cov) <- ""
    names(object$con$method) <- ""
    cat("Algorithm:", object$con$method)
    cat("\n")
    cat("Correction:", object$con$correct)
    cat("\n")
    cat("Number iterations:", iteration_cov)
}
