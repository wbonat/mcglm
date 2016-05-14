#' @title Derivative of C with respect to rho.
#' @author Wagner Hugo Bonat
#'
#'@description Compute the derivative of the C matrix with respect to
#'     the correlation parameters rho.
#'
#'@param D_Sigmab A matrix.
#'@param Bdiag_chol_Sigma_within A block-diagonal matrix.
#'@param t_Bdiag_chol_Sigma_within A block-diagonal matrix.
#'@param II A diagonal matrix.
#'@return A matrix.
#'@keywords internal
#'@details It is an internal function used to build the derivatives of
#'     the C matrix.

mc_derivative_C_rho <- function(D_Sigmab, Bdiag_chol_Sigma_within,
                                t_Bdiag_chol_Sigma_within, II) {
    output <- lapply(D_Sigmab,
                     function(x) {
                         t_Bdiag_chol_Sigma_within %*%
                             kronecker(x, II) %*%
                             Bdiag_chol_Sigma_within
                     })
    return(output)
}
