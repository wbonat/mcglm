#' @title Compute the cross-variability matrix
#' @author Wagner Hugo Bonat
#'
#' @description Compute the cross-variability matrix between covariance
#'     and regression parameters.
#'
#' @param Product_cov A list of matrices.
#' @param inv_C A matrix.
#' @param res A vector.
#' @param D A matrix.
#' @keywords internal
#' @return The cross-variability matrix between regression and
#'     covariance parameters.

mc_cross_variability <- function(Product_cov, inv_C, res, D) {
    Wlist <- lapply(Product_cov, mc_multiply2, bord2 = inv_C)
    A <- t(D) %*% inv_C
    n_beta <- dim(A)[1]
    n_cov <- length(Product_cov)
    cross_variability <- Matrix(NA, ncol = n_cov, nrow = n_beta)
    for (j in 1:n_beta) {
        for (i in 1:n_cov) {
            cross_variability[j, i] <-
                covprod(A[j, ], Wlist[[i]], res = res)
        }
    }
    return(cross_variability)
}
