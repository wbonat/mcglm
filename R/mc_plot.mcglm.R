#' Default Multivariate Covariance Generalized Linear models plotting
#'
#' @description takes a fitted mcglm object by \code{mcglm()} and plots, residuals,
#' influence diagnostic measures and algorithm check.
#'
#' @param x a fitted mcglm object as produced by \code{mcglm()}.
#' @param type Specify which graphical analysis will be performed, options are: residuals, influence
#' and algorithm.
#' @param ... additional arguments affecting the plot produced. Note that there is no extra options for
#' mcglm object class.
#' @export

plot.mcglm <- function(x, type = "residuals", ...) {
    object = x
    n_resp <- length(object$beta_names)
    if (type == "residuals") {
        par(mar = c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0), mfrow = c(2, n_resp))
        for (i in 1:n_resp) {
            res <- residuals(object, type = "pearson")[, i]
            fit_values <- fitted(object)[, i]
            plot(res ~ fit_values, ylab = "Pearson residuals", xlab = "Fitted values")
            temp <- loess.smooth(fitted(object)[, i], residuals(object, type = "pearson")[, i])
            lines(temp$x, temp$y)
            qqnorm(res)
            qqline(res)
        }
    }
    if (type == "algorithm") {
        n_iter <- length(na.exclude(object$IterationCovariance[, 1]))
        par(mar = c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0), mfrow = c(2, 2))
        matplot(object$IterationRegression[1:c(n_iter + 5), ], type = "l", lty = 2, ylab = "Regression", xlab = "Iterations")
        matplot(object$IterationCovariance[1:c(n_iter + 5), ], type = "l", lty = 2, ylab = "Covariance", xlab = "Iterations")
        matplot(object$ScoreRegression[1:c(n_iter + 5), ], type = "l", lty = 2, ylab = "Quasi-score Regression", xlab = "Iterations")
        matplot(object$ScoreCovariance[1:c(n_iter + 5), ], type = "l", lty = 2, ylab = "Quasi-score Covariance", xlab = "Iterations")
    }
    if (type == "partial_residuals") {
        list_beta <- mc_updateBeta(list_initial = object$list_initial, betas = object$Regression, n_resp = n_resp, information = object$Information)
        comp_X <- list()
        for (i in 1:n_resp) {
            comp_X[[i]] <- as.matrix(object$list_X[[i]]) * as.numeric(list_beta$regression[[i]])
        }
        for (i in 1:n_resp) {
            res <- residuals(object, type = "pearson")[, i]
            dev.new()
            n_cov <- dim(comp_X[[i]])[2]
            par(mar = c(2.6, 2.5, 0.5, 0.5), mgp = c(1.6, 0.6, 0), mfrow = c(1, c(n_cov - 1)))
            for (j in 2:n_cov) {
                p1 <- comp_X[[i]][, j] + res
                plot(p1 ~ object$list_X[[i]][, j], xlab = object$beta_names[[i]][j], ylab = "Partial residuals ")
            }
        }
    }
}
