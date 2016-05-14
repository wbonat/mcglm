#' @title Variability matrix
#' @author Wagner Hugo Bonat
#'
#' @description Compute the variability matrix associated with the
#'     Pearson estimating function.
#'
#' @param sensitivity A matrix. In general the output from
#'     \code{mc_sensitivity}.
#' @param product A list of matrix.
#' @param inv_C A matrix. In general the output from \code{mc_build_C}.
#' @param C A matrix. In general the output from \code{mc_build_C}.
#' @param res A vector. The residuals vector, i.e. (y_vec - mu_vec).
#' @return The variability matrix associated witht the Pearson
#'     estimating function.
#' @keywords internal
#' @details This function implements the equation 8 of Bonat and
#'     Jorgensen (2016).

mc_variability <- function(sensitivity, product, inv_C, C, res) {
    W <- lapply(product, mc_multiply2, bord2 = inv_C)
    n_par <- length(product)
    Variability <- matrix(NA, ncol = n_par, nrow = n_par)
    ## Take care here, I have found problems when using Matrix classes
    ## in this step. I do not know why yet!!
    k4 <- res^4 - 3 * diag(C)^2
    for (i in 1:n_par) {
        for (j in 1:n_par) {
            Variability[i, j] <-
                as.numeric(-2 * sensitivity[i, j] +
                                sum(k4 * diag(W[[i]]) * diag(W[[j]])))
        }
    }
    return(Variability)
}
