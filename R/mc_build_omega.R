#' @title Build omega matrix
#' @name mc_build_omega
#' @author Wagner Hugo Bonat
#'
#' @description This function build \eqn{\Omega} matrix according the
#'     covariance link function.
#'
#' @param tau A vector
#' @param Z A list of matrices.
#' @param covariance_link String specifing the covariance link function:
#'     identity, inverse, expm.
#' @param sparse Logical force to use sparse matrix representation
#'     'dsCMatrix'.
#' @keywords internal
#' @return A list with the \eqn{\Omega} matrix its inverse and
#'     derivatives with respect to \eqn{\tau}.

mc_build_omega <- function(tau, Z, covariance_link, sparse = FALSE) {
    if (covariance_link == "identity") {
        Omega <- mc_matrix_linear_predictor(tau = tau, Z = Z)
        output <- list(Omega = Omega, D_Omega = Z)
    }
    if (covariance_link == "expm") {
        U <- mc_matrix_linear_predictor(tau = tau, Z = Z)
        temp <- mc_expm(U = U, inverse = FALSE, sparse = sparse)
        D_Omega <- lapply(Z, mc_derivative_expm, UU = temp$UU,
                          inv_UU = temp$inv_UU, Q = temp$Q, sparse = sparse)
        output <- list(Omega = forceSymmetric(temp$Omega),
                       D_Omega = D_Omega)
    }
    if (covariance_link == "inverse") {
        inv_Omega <- mc_matrix_linear_predictor(tau = tau, Z = Z)
        output <- list(inv_Omega = inv_Omega, D_inv_Omega = Z)
    }
    return(output)
}
