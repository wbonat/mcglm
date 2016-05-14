#'@title Build the joint covariance matrix
#'@name mc_build_C
#'@author Wagner Hugo Bonat
#'
#'@description This function builds the joint variance-covariance matrix
#'     using the Generalized Kronecker product and its derivatives with
#'     respect to rho, power and tau parameters.
#'
#'@param list_mu A list with values of the mean.
#'@param list_Ntrial A list with the number of trials. Usefull only for
#'     binomial responses.
#'@param rho Vector of correlation parameters.
#'@param list_tau A list with values for the tau parameters.
#'@param list_power A list with values for the power parameters.
#'@param list_Z A list of matrix to be used in the matrix linear
#'     predictor.
#'@param list_sparse A list with Logical.
#'@param list_variance A list specifying the variance function to be
#'     used for each response variable.
#'@param list_covariance A list specifying the covariance function to be
#'     used for each response variable.
#'@param list_power_fixed A list of Logical specifying if the power
#'     parameters are fixed or not.
#'@param compute_C Logical. Compute or not the C matrix.
#'@param compute_derivative_beta Logical. Compute or not the derivative
#'     of C with respect to regression parameters.
#'@param compute_derivative_cov Logical. Compute or not the derivative
#'     of C with respect the covariance parameters.
#'@keywords internal
#'@return A list with the inverse of the C matrix and the derivatives of
#'     the C matrix with respect to rho, power and tau parameters.

mc_build_C <- function(list_mu, list_Ntrial, rho, list_tau, list_power,
                       list_Z, list_sparse, list_variance,
                       list_covariance, list_power_fixed,
                       compute_C = FALSE,
                       compute_derivative_beta = FALSE,
                       compute_derivative_cov = TRUE) {
    n_resp <- length(list_mu)
    n_obs <- length(list_mu[[1]][[1]])
    n_rho <- n_resp * (n_resp - 1)/2
    if (n_resp != 1) {
        assert_that(n_rho == length(rho))
    }
    list_Sigma_within <- suppressWarnings(
        Map(mc_build_sigma, mu = list_mu, Ntrial = list_Ntrial,
            tau = list_tau, power = list_power, Z = list_Z,
            sparse = list_sparse, variance = list_variance,
            covariance = list_covariance,
            power_fixed = list_power_fixed,
            compute_derivative_beta = compute_derivative_beta))
    list_Sigma_chol <- lapply(list_Sigma_within,
                              function(x) x$Sigma_chol)
    list_Sigma_inv_chol <- lapply(list_Sigma_within,
                                  function(x) x$Sigma_chol_inv)
    Sigma_between <- mc_build_sigma_between(rho = rho, n_resp = n_resp)
    II <- Diagonal(n_obs, 1)
    nucleo <- kronecker(Sigma_between$Sigmab, II)
    Bdiag_chol_Sigma_within <- bdiag(list_Sigma_chol)
    t_Bdiag_chol_Sigma_within <- t(Bdiag_chol_Sigma_within)
    Bdiag_inv_chol_Sigma <- bdiag(list_Sigma_inv_chol)
    inv_C <- Bdiag_inv_chol_Sigma %*%
        kronecker(solve(Sigma_between$Sigmab), II) %*%
        t(Bdiag_inv_chol_Sigma)
    output <- list(inv_C = inv_C)
    if (compute_derivative_cov == TRUE) {
        list_D_Sigma <- lapply(list_Sigma_within, function(x) x$D_Sigma)
        ## Derivatives of C with respect to power and tau parameters
        list_D_chol_Sigma <-
            Map(mc_derivative_cholesky, derivada = list_D_Sigma,
                inv_chol_Sigma = list_Sigma_inv_chol,
                chol_Sigma = list_Sigma_chol)
        mat_zero <- mc_build_bdiag(n_resp = n_resp, n_obs = n_obs)
        Bdiag_D_chol_Sigma <-
            mapply(mc_transform_list_bdiag,
                   list_mat = list_D_chol_Sigma,
                   response_number = 1:n_resp,
                   MoreArgs = list(mat_zero = mat_zero))
        Bdiag_D_chol_Sigma <- do.call(c, Bdiag_D_chol_Sigma)
        D_C <- lapply(Bdiag_D_chol_Sigma, mc_sandwich_cholesky,
                      middle = nucleo,
                      bord2 = t_Bdiag_chol_Sigma_within)
        ## Finish the derivatives with respect to power and tau
        ## parameters
        if (n_resp > 1) {
            D_C_rho <-
                mc_derivative_C_rho(D_Sigmab = Sigma_between$D_Sigmab,
                                    Bdiag_chol_Sigma_within =
                                        Bdiag_chol_Sigma_within,
                                    t_Bdiag_chol_Sigma_within =
                                        t_Bdiag_chol_Sigma_within,
                                    II = II)
            D_C <- c(D_C_rho, D_C)
        }
        output$D_C <- D_C
    }
    if (compute_C == TRUE) {
        C <- t_Bdiag_chol_Sigma_within %*%
            kronecker(Sigma_between$Sigmab, II) %*%
            Bdiag_chol_Sigma_within
        output$C <- C
    }
    if (compute_derivative_beta == TRUE) {
        list_D_Sigma_beta <- lapply(list_Sigma_within,
                                    function(x) x$D_Sigma_beta)
        list_D_chol_Sigma_beta <-
            Map(mc_derivative_cholesky, derivada = list_D_Sigma_beta,
                inv_chol_Sigma = list_Sigma_inv_chol,
                chol_Sigma = list_Sigma_chol)
        mat_zero <- mc_build_bdiag(n_resp = n_resp, n_obs = n_obs)
        Bdiag_D_chol_Sigma_beta <-
            mapply(mc_transform_list_bdiag,
                   list_mat = list_D_chol_Sigma_beta,
                   response_number = 1:n_resp,
                   MoreArgs = list(mat_zero = mat_zero))
        Bdiag_D_chol_Sigma_beta <- do.call(c, Bdiag_D_chol_Sigma_beta)
        D_C_beta <- lapply(Bdiag_D_chol_Sigma_beta,
                           mc_sandwich_cholesky, middle = nucleo,
                           bord2 = t_Bdiag_chol_Sigma_within)
        output$D_C_beta <- D_C_beta
    }
    return(output)
}
