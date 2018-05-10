#' @title Anova Tables
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Performs Wald tests of the significance for the linear
#' predictor components by response variables. This function is useful
#' for joint hypothesis tests of regression coefficients associated with
#' categorical covariates with more than two levels. It is not designed
#' for model comparison.
#'
#' @param object an object of class \code{mcglm}, usually, a result of a
#'     call to \code{mcglm()} function.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#'
#' @return A \code{data.frame} with Chi-square statistic to test the
#'     null hypothesis of a parameter, or a set of parameters, be
#'     zero. Degree of freedom (Df) and p-values.
#'     The Wald test based on the observed covariance matrix of
#'     the parameters is used.
#' @examples
#' x1 <- seq(-1, 1, l = 100)
#' x2 <- gl(5, 20)
#' beta <- c(5, 0, -2, -1, 1, 2)
#' X <- model.matrix(~ x1 + x2)
#' set.seed(123)
#' y <- rnorm(100, mean = X%*%beta, sd = 1)
#' data = data.frame("y" = y, "x1" = x1, "x2" = x2)
#' fit.anova <- mcglm(c(y ~ x1 + x2), list(mc_id(data)), data = data)
#' anova(fit.anova)
#'
#' @method anova mcglm
#' @export

anova.mcglm <- function(object, ...) {
    n_resp <- length(object$mu_list)
    n_beta <- lapply(object$list_X, ncol)
    idx.list <- list()
    for (i in 1:n_resp) {
        idx.list[[i]] <- rep(i, n_beta[i])
    }
    vv <- vcov(object)
    n_par <- dim(vv)[1]
    idx.vec <- do.call(c, idx.list)
    n_cov <- n_par - length(idx.vec)
    idx.vec <- c(idx.vec, rep(0, n_cov))
    temp.vcov <- list()
    temp.beta <- list()
    for (i in 1:n_resp) {
        idx.id <- idx.vec == i
        temp.vcov[[i]] <- vv[idx.id, idx.id]
        temp.beta[[i]] <-
            coef(object, type = "beta", response = i)$Estimates
    }
    saida <- list()
    for (i in 1:n_resp) {
        idx <- attr(object$list_X[[i]], "assign")
        names <- colnames(object$list_X[[i]])
        if (names[1] == "(Intercept)") {
            idx <- idx[-1]
            names <- names[-1]
            temp.beta[[i]] <- temp.beta[[i]][-1]
            temp.vcov[[i]] <- temp.vcov[[i]][-1, -1]
        }
        n_terms <- length(unique(idx))
        X2.resp <- list()
        for (j in 1:n_terms) {
            idx.TF <- idx == j
            temp <- as.numeric(
                t(temp.beta[[i]][idx.TF]) %*%
                    solve(as.matrix(temp.vcov[[i]])[idx.TF, idx.TF]) %*%
                    temp.beta[[i]][idx.TF])
            nbeta.test <- length(temp.beta[[i]][idx.TF])
            X2.resp[[j]] <-
                data.frame(Covariate = names[idx.TF][1],
                           Chi.Square = round(temp, 4), Df = nbeta.test,
                           p.value = round(pchisq(temp, nbeta.test,
                                                  lower.tail = FALSE),
                                           4))
        }
        saida[[i]] <- do.call(rbind, X2.resp)
    }
    cat("Wald test for fixed effects\n")
    for(i in 1:n_resp) {
      cat("Call: ")
      print(object$linear_pred[[i]])
      cat("\n")
      print(saida[[i]])
      cat("\n")
    }
    return(invisible(saida))
}

#' @title Model Coefficients
#' @name coef.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract model coefficients for objects
#'              of \code{mcglm} class.
#'
#' @param object an object of \code{mcglm} class.
#' @param std.error logical. If \code{TRUE} returns the standard errors
#'     for the estimates. Default is \code{FALSE}.
#' @param response a numeric vector specifyng for which response
#'     variable the coefficients should be returned.
#' @param type a string vector (can be 1 element length) specifying which
#'     coefficients should be returned. \cr
#'     Options are \code{"beta"}, \code{"tau"}, \code{"power"}, \code{"tau"} and
#'     \code{"correlation"}.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for \code{mcglm} object class.
#'
#' @return A \code{data.frame} with parameters names, estimates,
#'     response variable number and parameters type.
#'
#' @method coef mcglm
#' @export

coef.mcglm <- function(object, std.error = FALSE,
                       response = c(NA, 1:length(object$beta_names)),
                       type = c("beta", "tau", "power", "correlation"),
                       ...) {
    n_resp <- length(object$beta_names)
    cod_beta <- list()
    cod_power <- list()
    cod_tau <- list()
    type_beta <- list()
    type_power <- list()
    type_tau <- list()
    resp_beta <- list()
    resp_power <- list()
    resp_tau <- list()
    response_for <- 1:n_resp
    for (i in response_for) {
        cod_beta[[i]] <- paste0(
            paste0("beta", i), 0:c(object$Information$n_betas[[i]] - 1))
        type_beta[[i]] <- rep("beta", length(cod_beta[[i]]))
        resp_beta[[i]] <- rep(response_for[i], length(cod_beta[[i]]))
        if (object$Information$n_power[[i]] != 0 |
            object$power_fixed[[i]] == FALSE) {
            cod_power[[i]] <- paste0(
                paste0("power", i), 1:object$Information$n_power[[i]])
            type_power[[i]] <- rep("power",
                                   length(cod_power[[i]]))
            resp_power[[i]] <- rep(response_for[i],
                                   length(cod_power[[i]]))
        }
        if (object$Information$n_power[[i]] == 0) {
            cod_power[[i]] <- rep(1, 0)
            type_power[[i]] <- rep(1, 0)
            resp_power[[i]] <- rep(1, 0)
        }
        cod_tau[[i]] <- paste0(
            paste0("tau", i), 1:object$Information$n_tau[[i]])
        type_tau[[i]] <- rep("tau", length(cod_tau[[i]]))
        resp_tau[[i]] <- rep(response_for[i], length(cod_tau[[i]]))
    }
    rho_names <- c()
    if (n_resp != 1) {
        combination <- combn(n_resp, 2)
        for (i in 1:dim(combination)[2]) {
            rho_names[i] <- paste0(
                paste0("rho", combination[1, i]), combination[2, i])
        }
    }
    type_rho <- rep("correlation", length(rho_names))
    resp_rho <- rep(NA, length(rho_names))
    cod <- c(do.call(c, cod_beta), rho_names,
             do.call(c, Map(c, cod_tau)))
    type_cod <- c(do.call(c, type_beta), type_rho,
                  do.call(c, Map(c, type_tau)))
    response_cod <- c(do.call(c, resp_beta), resp_rho,
                      do.call(c, Map(c, resp_tau)))

    if (length(cod_power) != 0) {
        cod <- c(do.call(c, cod_beta), rho_names,
                 do.call(c, Map(c, cod_power, cod_tau)))
        type_cod <- c(do.call(c, type_beta), type_rho,
                      do.call(c, Map(c, type_power, type_tau)))
        response_cod <- c(do.call(c, resp_beta), resp_rho,
                          do.call(c, Map(c, resp_power, resp_tau)))
    }

    Estimates <- c(object$Regression, object$Covariance)
    coef_temp <- data.frame(
        Estimates = Estimates,
        Parameters = cod,
        Type = type_cod,
        Response = response_cod)
    if (std.error == TRUE) {
        coef_temp <- data.frame(
            Estimates = Estimates,
            Std.error = sqrt(diag(object$vcov)),
            Parameters = cod, Type = type_cod,
            Response = response_cod)
    }
    output <- coef_temp[
        which(coef_temp$Response %in% response &
                  coef_temp$Type %in% type), ]
    return(output)
}

#' @title Confidence Intervals for Model Parameters
#' @name confint.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Computes confidence intervals for parameters in a fitted
#'     \code{mcglm} model.
#'
#' @param object a fitted \code{mcglm} object.
#' @param parm specifies for which parameters are to be given
#'     confidence intervals, either a vector of numbers or a vector of
#'     strings. If missing, all parameters are considered.
#' @param level the nominal confidence level.
#' @param ... additional arguments affecting the confidence interval
#'     produced. Note that there is no extra options for \code{mcglm}
#'     object class.
#'
#' @return A \code{data.frame} with confidence intervals, parameters
#'     names, response variable number and parameters type.
#' @method confint mcglm
#' @export

confint.mcglm <- function(object, parm, level = 0.95, ...) {
    temp <- coef(object, std.error = TRUE)
    if (missing(parm)) {
        parm <- 1:length(temp$Estimates)
    }
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- stats::qnorm(a)
    ci <- temp$Estimates + temp$Std.error %o% fac
    colnames(ci) <- paste0(format(a, 2), "%")
    rownames(ci) <- temp$Parameters
    return(ci[parm, ])
}

#' @title Fitted Values
#' @name fitted.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract fitted values for objects of \code{mcglm} class.
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for \code{mcglm} object class.
#'
#' @return A matrix with fitted values.
#'
#' @method fitted mcglm
#' @export

fitted.mcglm <- function(object, ...) {
    n_resp <- length(object$beta_names)
    output <- Matrix(object$fitted, ncol = n_resp, nrow = object$n_obs)
    return(output)
}

#' @title Residuals and algorithm check plots
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Residual and algorithm check analysis for objects of
#' \code{mcglm} class.
#'
#' @param x a fitted \code{mcglm} object.
#' @param type specify which graphical analysis will be performed.
#'     Options are: \code{"residuals"} and \code{"algorithm"}.
#' @param ... additional arguments affecting the plot produced. Note
#'     that there is no extra options for mcglm object class.
#'
#' @return The function \code{plot.mcglm} was designed to offer a fast
#' residuals analysis based on the Pearson residuals. Current version
#' offers a simple Pearson residuals versus fitted values and a quantile
#' plot. When using \code{algorithm = TRUE} the function will plot a summary of
#' the fitting algorithm shows the trajectory or iterations of the fitting
#' algorithm. The iterations are shown in terms of values of the model
#' parameters and also the actually value of the quasi-score and Pearson
#' estimating functions. Hence, a quickly check of the algorithm
#' convergence is obtained.
#' @seealso \code{residuals} and \code{fitted}.
#' @method plot mcglm
#' @export

plot.mcglm <- function(x, type = "residuals", ...) {
    object <- x
    n_resp <- length(object$beta_names)
    if (type == "residuals") {
        graphics::par(mar = c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0),
            mfrow = c(2, n_resp))
        for (i in 1:n_resp) {
            res <- residuals(object, type = "pearson")[, i]
            fit_values <- fitted(object)[, i]
            graphics::plot(res ~ fit_values, ylab = "Pearson residuals",
                 xlab = "Fitted values")
            temp <-
                stats::loess.smooth(fitted(object)[, i],
                             residuals(object, type = "pearson")[, i])
            graphics::lines(temp$x, temp$y)
            stats::qqnorm(res)
            stats::qqline(res)
        }
    }
    if (type == "algorithm") {
        n_iter <- length(na.exclude(object$IterationCovariance[,
                                                               1]))
        graphics::par(mar = c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0),
            mfrow = c(2, 2))
        graphics::matplot(object$IterationRegression[1:c(n_iter + 5), ],
                type = "l", lty = 2, ylab = "Regression",
                xlab = "Iterations")
        graphics::matplot(object$IterationCovariance[1:c(n_iter + 5), ],
                type = "l", lty = 2, ylab = "Covariance",
                xlab = "Iterations")
        graphics::matplot(object$ScoreRegression[1:c(n_iter + 5), ], type = "l",
                lty = 2, ylab = "Quasi-score Regression",
                xlab = "Iterations")
        graphics::matplot(object$ScoreCovariance[1:c(n_iter + 5), ], type = "l",
                lty = 2, ylab = "Quasi-score Covariance",
                xlab = "Iterations")
    }
    if (type == "partial_residuals") {
        list_beta <- mc_updateBeta(list_initial = object$list_initial,
                                   betas = object$Regression,
                                   n_resp = n_resp,
                                   information = object$Information)
        comp_X <- list()
        for (i in 1:n_resp) {
            comp_X[[i]] <- as.matrix(object$list_X[[i]]) *
                as.numeric(list_beta$regression[[i]])
        }
        for (i in 1:n_resp) {
            res <- residuals(object, type = "pearson")[, i]
            dev.new()
            n_cov <- dim(comp_X[[i]])[2]
            graphics::par(mar = c(2.6, 2.5, 0.5, 0.5),
                mgp = c(1.6, 0.6, 0),
                mfrow = c(1, c(n_cov - 1)))
            for (j in 2:n_cov) {
                p1 <- comp_X[[i]][, j] + res
                graphics::plot(p1 ~ object$list_X[[i]][, j],
                     xlab = object$beta_names[[i]][j],
                     ylab = "Partial residuals ")
            }
        }
    }
}

#' @title Print
#' @name print.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The default print method for an object of \code{mcglm} class.
#'
#' @param x fitted model objects of class \code{mcglm} as produced by \code{mcglm()}.
#' @param ... further arguments passed to or from other methods.
#'
#' @seealso \code{summary}.
#' @rdname print.mcglm
#' @method print mcglm
#' @export

print.mcglm <- function(x, ...) {
    object <- x
    n_resp <- length(object$beta_names)
    regression <- mc_updateBeta(list_initial = list(),
                                betas = object$Regression,
                                information = object$Information,
                                n_resp = n_resp)
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
        tau_temp <- coef(object, response = i,
                         type = "tau")$Estimate
        names(tau_temp) <- rep("", length(tau_temp))
        print(tau_temp)
        cat("\n")
        power_temp <- coef(object, response = i,
                           type = "power")$Estimate
        if (length(power_temp) != 0) {
            names(power_temp) <- ""
            cat("Power:\n")
            print(power_temp)
            cat("\n")
        }
    }
}

#' @title Residuals
#' @name residuals.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Compute residuals for an object of \code{mcglm} class.
#'
#' @param object an object of \code{mcglm} class.
#' @param type the type of residuals which should be returned.
#'     Options are: \code{"raw"} (default), \code{"pearson"} and
#'     \code{"standardized"}.
#' @param ... additional arguments affecting the residuals
#'     produced. Note that there is no extra options for mcglm object
#'     class.
#'
#' @return The function \code{residuals.mcglm} returns a matrix of
#' residuals values.
#' @seealso \code{fitted}.
#' @method residuals mcglm
#' @export

residuals.mcglm <- function(object, type = "raw", ...) {
    n_resp <- length(object$beta_names)
    output <- Matrix(object$residuals, ncol = n_resp, nrow = object$n_obs)
    if (type == "standardized") {
        output <- Matrix(
            as.numeric(object$residuals %*% chol(object$inv_C)),
            ncol = n_resp, nrow = object$n_obs)
    }
    if (type == "pearson") {
        output <- Matrix(
            as.numeric(object$residuals/sqrt(diag(object$C))),
            ncol = n_resp, nrow = object$n_obs)
    }
    return(output)
}

#' @title Summarizing
#' @name summary.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The default summary method for an object of \code{mcglm} class.
#'
#' @param object an object of \code{mcglm} class.
#' @param verbose logical. Print or not the model summary.
#' @param print print only part of the model summary, options are
#' \code{Regression}, \code{power}, \code{Dispersion} and \code{Correlation}.
#' @param ... additional arguments affecting the summary produced. Note
#'     the there is no extra options for mcglm object class.
#'
#' @return Print a \code{mcglm} object.
#' @seealso \code{print}.
#' @method summary mcglm
#' @export

summary.mcglm <- function(object, verbose = TRUE,
                          print = c("Regression","power","Dispersion","Correlation"),
                          ...) {
    n_resp <- length(object$beta_names)
    output <- list()
    for(i in 1:n_resp) {
    tab_beta <- coef(object, std.error = TRUE,
                     response = i, type = "beta")[, 1:2]
    tab_beta$"Z value" <- tab_beta[, 1]/tab_beta[, 2]
    tab_beta$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_beta[, 1]/tab_beta[, 2]))
    rownames(tab_beta) <- object$beta_names[[i]]
    output[i][[1]]$Regression <- tab_beta
    tab_power <- coef(object, std.error = TRUE,
                      response = i, type = "power")[, 1:2]
    tab_power$"Z value" <- tab_power[, 1]/tab_power[, 2]
    tab_power$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_power[, 1]/tab_power[, 2]))
    rownames(tab_power) <- NULL
    if (dim(tab_power)[1] != 0) {
      output[i][[1]]$Power <- tab_power
    }
    tab_tau <- coef(object, std.error = TRUE,
                    response = i, type = "tau")[, 1:2]
    tab_tau$"Z value" <- tab_tau[, 1]/tab_tau[, 2]
    tab_tau$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_tau[, 1]/tab_tau[, 2]))
    rownames(tab_tau) <- NULL
    output[i][[1]]$tau <- tab_tau
    }
    tab_rho <- coef(object, std.error = TRUE,
                    response = NA, type = "correlation")[, c(3, 1, 2)]
    tab_rho$"Z value" <- tab_rho[, 2]/tab_rho[, 3]
    tab_rho$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_rho[, 2]/tab_rho[, 3]))
    if (dim(tab_rho)[1] != 0) {
      rownames(tab_rho) <- NULL
      output$Correlation <- tab_rho
    }
    if(verbose == TRUE) {
    for (i in 1:n_resp) {
      if("Regression" %in% print) {
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
        print(output[i][[1]]$Regression)
        cat("\n")
      }
        if (dim(tab_power)[1] != 0) {
          if("power" %in% print) {
            cat("Power:\n")
            print(output[i][[1]]$Power)
           cat("\n")
          }
        }
      if("Dispersion" %in% print) {
        cat("Dispersion:\n")
        print(output[i][[1]]$tau)
        cat("\n")
      }
    }
    if (dim(tab_rho)[1] != 0) {
      if("Correlation" %in% print) {
        cat("Correlation matrix:\n")
        print(tab_rho)
        cat("\n")
      }
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
    cat("\n")
    }
    if (dim(tab_rho)[1] != 0) {
    names(output) <- c(paste("Resp.Variable", 1:n_resp),"Correlation")
    }
    if (dim(tab_rho)[1] == 0) {
      names(output) <- paste("Resp.Variable", 1:n_resp)
    }
    return(invisible(output))
}

#' @title Variance-Covariance Matrix
#' @name vcov.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Returns the variance-covariance matrix for an object
#' of \code{mcglm} class.
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#'
#' @return A variance-covariance matrix.
#'
#' @method vcov mcglm
#' @export

vcov.mcglm <- function(object, ...) {
    cod <- coef(object)$Parameters
    colnames(object$vcov) <- cod
    rownames(object$vcov) <- cod
    return(object$vcov)
}
