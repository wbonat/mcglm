#' @title Build variance-covariance matrix
#' @author Wagner Hugo Bonat
#'
#' @description This function builds a variance-covariance matrix, based
#'     on the variance function and omega matrix.
#'
#'@param mu A numeric vector. In general the output from
#'     \code{\link{mc_link_function}}.
#'@param Ntrial A numeric vector, or NULL or a numeric specifing the
#'     number of trials in the binomial experiment. It is usefull only
#'     when using variance = binomialP or binomialPQ. In the other cases
#'     it will be ignored.
#'@param tau A numeric vector.
#'@param power A numeric or numeric vector. It should be one number for
#'     all variance functions except binomialPQ, in that case the
#'     argument specifies both p and q.
#'@param Z A list of matrices.
#'@param sparse Logical.
#'@param variance String specifing the variance function: constant,
#'     tweedie, poisson_tweedie, binomialP or binomialPQ.
#'@param covariance String specifing the covariance function: identity,
#'     inverse or expm.
#'@param power_fixed Logical if the power parameter is fixed at initial
#'     value (TRUE). In the case power_fixed = FALSE the power parameter
#'     will be estimated.
#'@param compute_derivative_beta Logical. Compute or not the derivative
#'     with respect to regression parameters.
#'@keywords internal
#'@return A list with the Cholesky decomposition of \eqn{\Sigma},
#'     \eqn{\Sigma^{-1}} and the derivative of \eqn{\Sigma} with respect
#'     to the power and tau parameters.
#'@seealso \code{\link{mc_link_function}},
#'     \code{\link{mc_variance_function}}, \code{\link{mc_build_omega}}.

mc_build_sigma <- function(mu, Ntrial = 1, tau, power, Z, sparse,
                           variance, covariance, power_fixed,
                           compute_derivative_beta = FALSE) {
    if (variance == "constant") {
        if (covariance == "identity" | covariance == "expm") {
            Omega <- mc_build_omega(tau = tau, Z = Z,
                                    covariance_link = covariance,
                                    sparse = sparse)
            chol_Sigma <- chol(Omega$Omega)
            inv_chol_Sigma <- solve(chol_Sigma)
            output <- list(Sigma_chol = chol_Sigma,
                           Sigma_chol_inv = inv_chol_Sigma,
                           D_Sigma = Omega$D_Omega)
        }
        if (covariance == "inverse") {
            inv_Sigma <- mc_build_omega(tau = tau, Z = Z,
                                        covariance_link = "inverse",
                                        sparse = sparse)
            chol_inv_Sigma <- chol(inv_Sigma$inv_Omega)
            chol_Sigma <- solve(chol_inv_Sigma)
            ## Because a compute the inverse of chol_inv_Omega
            Sigma <- (chol_Sigma) %*% t(chol_Sigma)
            D_Sigma <- lapply(inv_Sigma$D_inv_Omega,
                           mc_sandwich_negative,
                              bord1 = Sigma, bord2 = Sigma)
            output <- list(Sigma_chol = t(chol_Sigma),
                           Sigma_chol_inv = t(chol_inv_Sigma),
                           D_Sigma = D_Sigma)
        }
    }

    if (variance == "tweedie" | variance == "binomialP" |
            variance == "binomialPQ") {
        if (variance == "tweedie") {
            variance <- "power"
        }
        if (covariance == "identity" | covariance == "expm") {
            Omega <- mc_build_omega(tau = tau, Z = Z,
                                    covariance_link = covariance,
                                    sparse = sparse)
            V_sqrt <- mc_variance_function(
                mu = mu$mu, power = power,
                Ntrial = Ntrial,
                variance = variance,
                inverse = FALSE,
                derivative_power = !power_fixed,
                derivative_mu = compute_derivative_beta)
            Sigma <- forceSymmetric(V_sqrt$V_sqrt %*% Omega$Omega %*%
                                        V_sqrt$V_sqrt)
            chol_Sigma <- chol(Sigma)
            inv_chol_Sigma <- solve(chol_Sigma)
            D_Sigma <- lapply(Omega$D_Omega, mc_sandwich,
                              bord1 = V_sqrt$V_sqrt,
                              bord2 = V_sqrt$V_sqrt)
            if (power_fixed == FALSE) {
                if (variance == "power" | variance == "binomialP") {
                    D_Sigma_power <- mc_sandwich_power(
                        middle = Omega$Omega, bord1 = V_sqrt$V_sqrt,
                        bord2 = V_sqrt$D_V_sqrt_p)
                    D_Sigma <- c(D_Sigma_power = D_Sigma_power,
                                 D_Sigma_tau = D_Sigma)
                }
                if (variance == "binomialPQ") {
                    D_Sigma_p <- mc_sandwich_power(
                        middle = Omega$Omega,
                        bord1 = V_sqrt$V_sqrt,
                        bord2 = V_sqrt$D_V_sqrt_p)
                    D_Sigma_q <- mc_sandwich_power(
                        middle = Omega$Omega,
                        bord1 = V_sqrt$V_sqrt,
                        bord2 = V_sqrt$D_V_sqrt_q)
                    D_Sigma <- c(D_Sigma_p, D_Sigma_q, D_Sigma)
                }
            }
            output <- list(Sigma_chol = chol_Sigma,
                           Sigma_chol_inv = inv_chol_Sigma,
                           D_Sigma = D_Sigma)
            if (compute_derivative_beta == TRUE) {
                D_Sigma_beta <- mc_derivative_sigma_beta(
                    D = mu$D, D_V_sqrt_mu = V_sqrt$D_V_sqrt_mu,
                    Omega = Omega$Omega, V_sqrt = V_sqrt$V_sqrt,
                    variance = variance)
                output$D_Sigma_beta <- D_Sigma_beta
            }
        }
        if (covariance == "inverse") {
            inv_Omega <- mc_build_omega(tau = tau, Z = Z,
                                        covariance_link = "inverse",
                                        sparse = sparse)
            V_inv_sqrt <- mc_variance_function(
                mu = mu$mu, power = power, Ntrial = Ntrial,
                variance = variance, inverse = TRUE,
                derivative_power = !power_fixed,
                derivative_mu = compute_derivative_beta)
            inv_Sigma <- forceSymmetric(V_inv_sqrt$V_inv_sqrt %*%
                                            inv_Omega$inv_Omega %*%
                                            V_inv_sqrt$V_inv_sqrt)
            inv_chol_Sigma <- chol(inv_Sigma)
            chol_Sigma <- solve(inv_chol_Sigma)
            Sigma <- chol_Sigma %*% t(chol_Sigma)
            D_inv_Sigma <- lapply(inv_Omega$D_inv_Omega, mc_sandwich,
                                  bord1 = V_inv_sqrt$V_inv_sqrt,
                                  bord2 = V_inv_sqrt$V_inv_sqrt)
            D_Sigma <- lapply(D_inv_Sigma, mc_sandwich_negative,
                              bord1 = Sigma, bord2 = Sigma)
            if (power_fixed == FALSE) {
                if (variance == "power" | variance == "binomialP") {
                    D_Omega_p <- mc_sandwich_power(
                        middle = inv_Omega$inv_Omega,
                        bord1 = V_inv_sqrt$V_inv_sqrt,
                        bord2 = V_inv_sqrt$D_V_inv_sqrt_power)
                    D_Sigma_p <- mc_sandwich_negative(
                        middle = D_Omega_p,
                        bord1 = Sigma, bord2 = Sigma)
                    D_Sigma <- c(D_Sigma_p, D_Sigma)
                }
                if (variance == "binomialPQ") {
                    D_Omega_p <- mc_sandwich_power(
                        middle = inv_Omega$inv_Omega,
                        bord1 = V_inv_sqrt$V_inv_sqrt,
                        bord2 = V_inv_sqrt$D_V_inv_sqrt_p)
                    D_Sigma_p <- mc_sandwich_negative(
                        middle = D_Omega_p,
                        bord1 = Sigma, bord2 = Sigma)
                    D_Omega_q <- mc_sandwich_power(
                        middle = inv_Omega$inv_Omega,
                        bord1 = V_inv_sqrt$V_inv_sqrt,
                        bord2 = V_inv_sqrt$D_V_inv_sqrt_q)
                    D_Sigma_q <- mc_sandwich_negative(
                        middle = D_Omega_q,
                        bord1 = Sigma, bord2 = Sigma)
                    D_Sigma <- c(D_Sigma_p, D_Sigma_q, D_Sigma)
                }
            }
            output <- list(Sigma_chol = t(chol_Sigma),
                           Sigma_chol_inv = t(inv_chol_Sigma),
                           D_Sigma = D_Sigma)
            if (compute_derivative_beta == TRUE) {
                D_inv_Sigma_beta <- mc_derivative_sigma_beta(
                    D = mu$D,
                    D_V_sqrt_mu = V_inv_sqrt$D_V_inv_sqrt_mu,
                    Omega = inv_Omega$inv_Omega,
                    V_sqrt = V_inv_sqrt$V_inv_sqrt, variance = variance)
                D_Sigma_beta <- lapply(D_inv_Sigma_beta,
                                       mc_sandwich_negative,
                                       bord1 = Sigma, bord2 = Sigma)
                output$D_Sigma_beta <- D_Sigma_beta
            }
        }
    }

    if (variance == "poisson_tweedie") {
        if (covariance == "identity" | covariance == "expm") {
            Omega <- mc_build_omega(tau = tau, Z = Z,
                                    covariance_link = covariance,
                                    sparse = sparse)
            V_sqrt <- mc_variance_function(
                mu = mu$mu, power = power,
                Ntrial = Ntrial, variance = "power", inverse = FALSE,
                derivative_power = !power_fixed,
                derivative_mu = compute_derivative_beta)
            Sigma <- forceSymmetric(Diagonal(length(mu$mu), mu$mu) +
                                        V_sqrt$V_sqrt %*%
                                        Omega$Omega %*% V_sqrt$V_sqrt)
            chol_Sigma <- chol(Sigma)
            inv_chol_Sigma <- solve(chol_Sigma)
            D_Sigma <- lapply(Omega$D_Omega, mc_sandwich,
                              bord1 = V_sqrt$V_sqrt,
                              bord2 = V_sqrt$V_sqrt)
            if (power_fixed == FALSE) {
                D_Sigma_power <- mc_sandwich_power(
                    middle = Omega$Omega,
                    bord1 = V_sqrt$V_sqrt, bord2 = V_sqrt$D_V_sqrt_p)
                D_Sigma <- c(D_Sigma_power = D_Sigma_power,
                             D_Sigma_tau = D_Sigma)
            }
            output <- list(Sigma_chol = chol_Sigma,
                           Sigma_chol_inv = inv_chol_Sigma,
                           D_Sigma = D_Sigma)
            if (compute_derivative_beta == TRUE) {
                D_Sigma_beta <- mc_derivative_sigma_beta(
                    D = mu$D,
                    D_V_sqrt_mu = V_sqrt$D_V_sqrt_mu, Omega$Omega,
                    V_sqrt = V_sqrt$V_sqrt, variance = variance)
                output$D_Sigma_beta <- D_Sigma_beta
            }
        }
        if (covariance == "inverse") {
            inv_Omega <- mc_build_omega(tau = tau, Z = Z,
                                        covariance_link = "inverse",
                                        sparse = sparse)
            Omega <- chol2inv(chol(inv_Omega$inv_Omega))
            V_sqrt <- mc_variance_function(
                mu = mu$mu, power = power,
                Ntrial = Ntrial, variance = "power", inverse = FALSE,
                derivative_power = !power_fixed,
                derivative_mu = compute_derivative_beta)
            D_Omega <- lapply(inv_Omega$D_inv_Omega,
                              mc_sandwich_negative, bord1 = Omega,
                              bord2 = Omega)
            D_Sigma <- lapply(D_Omega, mc_sandwich,
                              bord1 = V_sqrt$V_sqrt,
                              bord2 = V_sqrt$V_sqrt)
            Sigma <- forceSymmetric(Diagonal(length(mu$mu), mu$mu) +
                                        V_sqrt$V_sqrt %*% Omega %*%
                                        V_sqrt$V_sqrt)
            chol_Sigma <- chol(Sigma)
            inv_chol_Sigma <- solve(chol_Sigma)
            if (power_fixed == FALSE) {
                D_Sigma_p <- mc_sandwich_power(
                    middle = Omega,
                    bord1 = V_sqrt$V_sqrt,
                    bord2 = V_sqrt$D_V_sqrt_power)
                D_Sigma <- c(D_Sigma_p, D_Sigma)
            }
            output <- list(Sigma_chol = chol_Sigma,
                           Sigma_chol_inv = inv_chol_Sigma,
                           D_Sigma = D_Sigma)
            if (compute_derivative_beta == TRUE) {
                D_Sigma_beta <- mc_derivative_sigma_beta(
                    D = mu$D,
                    D_V_sqrt_mu = V_sqrt$D_V_sqrt_mu, Omega = Omega,
                    V_sqrt = V_sqrt$V_sqrt, variance = variance)
                output$D_Sigma_beta <- D_Sigma_beta
            }
        }
    }
    if(variance == "geom_tweedie") {
        if (covariance == "identity" | covariance == "expm") {
          Omega <- mc_build_omega(tau = tau, Z = Z,
                                  covariance_link = covariance,
                                  sparse = sparse)
          V_sqrt <- mc_variance_function(
            mu = mu$mu, power = power,
            Ntrial = Ntrial, variance = "power", inverse = FALSE,
            derivative_power = !power_fixed,
            derivative_mu = compute_derivative_beta)
          Sigma <- forceSymmetric(Diagonal(length(mu$mu), mu$mu^2) +
                                    V_sqrt$V_sqrt %*%
                                    Omega$Omega %*% V_sqrt$V_sqrt)
          chol_Sigma <- chol(Sigma)
          inv_chol_Sigma <- solve(chol_Sigma)
          D_Sigma <- lapply(Omega$D_Omega, mc_sandwich,
                            bord1 = V_sqrt$V_sqrt,
                            bord2 = V_sqrt$V_sqrt)
          if (power_fixed == FALSE) {
            D_Sigma_power <- mc_sandwich_power(
              middle = Omega$Omega,
              bord1 = V_sqrt$V_sqrt, bord2 = V_sqrt$D_V_sqrt_p)
            D_Sigma <- c(D_Sigma_power = D_Sigma_power,
                         D_Sigma_tau = D_Sigma)
          }
          output <- list(Sigma_chol = chol_Sigma,
                         Sigma_chol_inv = inv_chol_Sigma,
                         D_Sigma = D_Sigma)
          if (compute_derivative_beta == TRUE) {
            D_Sigma_beta <- mc_derivative_sigma_beta(
              D = mu$D,
              D_V_sqrt_mu = V_sqrt$D_V_sqrt_mu, Omega$Omega,
              V_sqrt = V_sqrt$V_sqrt, variance = variance)
            output$D_Sigma_beta <- D_Sigma_beta
          }
        }
        if (covariance == "inverse") {
          inv_Omega <- mc_build_omega(tau = tau, Z = Z,
                                      covariance_link = "inverse",
                                      sparse = sparse)
          Omega <- chol2inv(chol(inv_Omega$inv_Omega))
          V_sqrt <- mc_variance_function(
            mu = mu$mu, power = power,
            Ntrial = Ntrial, variance = "power", inverse = FALSE,
            derivative_power = !power_fixed,
            derivative_mu = compute_derivative_beta)
          D_Omega <- lapply(inv_Omega$D_inv_Omega,
                            mc_sandwich_negative, bord1 = Omega,
                            bord2 = Omega)
          D_Sigma <- lapply(D_Omega, mc_sandwich,
                            bord1 = V_sqrt$V_sqrt,
                            bord2 = V_sqrt$V_sqrt)
          Sigma <- forceSymmetric(Diagonal(length(mu$mu), mu$mu^2) +
                                    V_sqrt$V_sqrt %*% Omega %*%
                                    V_sqrt$V_sqrt)
          chol_Sigma <- chol(Sigma)
          inv_chol_Sigma <- solve(chol_Sigma)
          if (power_fixed == FALSE) {
            D_Sigma_p <- mc_sandwich_power(
              middle = Omega,
              bord1 = V_sqrt$V_sqrt,
              bord2 = V_sqrt$D_V_sqrt_power)
            D_Sigma <- c(D_Sigma_p, D_Sigma)
          }
          output <- list(Sigma_chol = chol_Sigma,
                         Sigma_chol_inv = inv_chol_Sigma,
                         D_Sigma = D_Sigma)
          if (compute_derivative_beta == TRUE) {
            D_Sigma_beta <- mc_derivative_sigma_beta(
              D = mu$D,
              D_V_sqrt_mu = V_sqrt$D_V_sqrt_mu, Omega = Omega,
              V_sqrt = V_sqrt$V_sqrt, variance = variance)
            output$D_Sigma_beta <- D_Sigma_beta
          }
        }
    }
    return(output)
}
