#' @title Compute the score information criterion (SIC) for multivariate
#'     covariance generalized linear models.
#' @author Wagner Hugo Bonat
#'
#' @description Compute SIC for covariance parameters in McGLMS.
#'
#' @param object an object representing a model of \code{mcglm} class.
#' @param scope a list of matrices to be tested in the matrix linear
#'     predictor.
#' @param idx Indicator of matrices belong to the same effect.
#' @param data data frame containing all variables envolved in the
#'     model.
#' @param penalty penalty term (default = 2).
#' @param response Indicate for which response variable SIC is computed.
#' @return A data frame with SIC values for each matrix in the scope
#'     argument.
#' @export

mc_sic_covariance <- function(object, scope, idx, data, penalty = 2,
                              response) {
    SIC <- c()
    df <- c()
    df_total <- c()
    TU <- c()
    QQ <- c()
    n_terms <- length(unique(idx))
    for (j in 1:n_terms) {
        tau <- coef(object, type = "tau", response = response)$Estimates
        n_tau <- length(tau)
        n_tau_new <- length(idx[idx == j])
        list_tau_new <- list(c(tau, rep(0, n_tau_new)))
        n_tau_total <- n_tau + n_tau_new
        if (object$power_fixed[[response]]) {
            list_power <- object$list_initial$power
        } else {
            list_power <- list(coef(object, type = "power",
                                    response = response)$Estimates)
            n_tau_total <- n_tau_total + 1
            n_tau <- n_tau + 1
        }
        list_Z_new <- list(c(object$matrix_pred[[response]],
                             scope[idx == j]))
        if (length(object$mu_list) == 1) {
            rho <- 0
        } else {
            rho <- coef(object, type = "correlation")$Estimates
        }
        Cfeatures <- mc_build_C(list_mu = object$mu_list,
                                list_Ntrial = object$Ntrial, rho = rho,
                                list_tau = list_tau_new,
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
        Sigma22 <- Sigma[c(n_tau + 1):n_tau_total,
                         c(n_tau + 1):n_tau_total]
        J21 <- J[c(n_tau + 1):n_tau_total, 1:n_tau]
        J11 <- solve(J[1:n_tau, 1:n_tau])
        Sigma12 <- Sigma[1:n_tau, c(n_tau + 1):n_tau_total]
        Sigma21 <- Sigma[c(n_tau + 1):n_tau_total, 1:n_tau]
        J12 <- J[1:n_tau, c(n_tau + 1):n_tau_total]
        Sigma11 <- Sigma[1:n_tau, 1:n_tau]

        V2 <- Sigma22 - J21 %*% J11 %*% Sigma12 - Sigma21 %*%
            J11 %*% J12 + J21 %*% J11 %*% Sigma11 %*% J11 %*%
            J12
        TU[j] <- t(temp_score$Score[c(n_tau + 1):n_tau_total] %*%
                   solve(V2) %*%
                   temp_score$Score[c(n_tau + 1):n_tau_total])
        df[j] <- n_tau_new
        SIC[j] <- -as.numeric(TU[j]) + penalty * n_tau_total
        QQ[j] <- qchisq(0.95, df = n_tau_new)
        df_total[j] <- n_tau_total
        #print(j)
    }
    output <- data.frame(SIC = SIC, df = df, df_total = df_total,
                         Tu = TU, Chisq = QQ)
    return(output)
}
