#' @title Cross-sensitivity
#' @author Wagner Hugo Bonat
#'
#' @description Compute the cross-sensitivity matrix between regression
#'     and covariance parameters.  Equation 10 of Bonat and Jorgensen
#'     (2015).
#'
#' @param Product_cov A list of matrices.
#' @param Product_beta A list of matrices.
#' @param n_beta_effective Numeric. Effective number of regression
#'     parameters.
#' @keywords internal
#' @return The cross-sensitivity matrix. Equation (10) of Bonat and
#'     Jorgensen (2015).

mc_cross_sensitivity <- function(Product_cov, Product_beta,
                                 n_beta_effective =
                                     length(Product_beta)) {
    n_beta <- length(Product_beta)
    n_cov <- length(Product_cov)
    if (n_beta == 0) {
        cross_sensitivity <- Matrix(0, ncol = n_beta_effective,
                                    nrow = n_cov)
    }
    if (n_beta != 0) {
        cross_sensitivity <- Matrix(NA, nrow = n_cov, ncol = n_beta)
        for (i in 1:n_cov) {
            for (j in 1:n_beta) {
                cross_sensitivity[i, j] <-
                    -sum(Product_cov[[i]] * Product_beta[[j]])
            }
        }
    }
    return(cross_sensitivity)
}
