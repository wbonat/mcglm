#' @title Variability matrix
#' @author Wagner Hugo Bonat and Eduardo Elias Ribeiro Jr
#'
#' @description Compute the variability matrix associated with the
#'     Pearson estimating function.
#'
#' @param product2 A list of matrix.
#' @param product A list of matrix.
#' @param inv_C A matrix. In general the output from \code{mc_build_C}.
#' @param res A vector. The residuals vector, i.e. (y_vec - mu_vec).
#' @param W Matrix of weights.
#' @return The variability matrix associated witht the Pearson
#'     estimating function.
#' @keywords internal
#' @details This function implements the equation 8 of Bonat and
#'     Jorgensen (2016).

mc_variability <- function(product2, product, inv_C, C, res, W) {
    WE <- lapply(product, mc_multiply2, bord2 = inv_C)
    n_par <- length(product)
    k4 <- res^4 - 3 * diag(C)^2
    #Variability <- matrix(NA, nrow = n_par, ncol = n_par)
    #for (i in 1:n_par) {
    #    for (j in 1:n_par) {
    #        Variability[i, j] <-
    #            as.numeric(-2 * sensitivity[i, j] +
    #                            sum(k4 * diag(W[[i]]) * diag(W[[j]])))
    #    }
    #}

    #sourceCpp("src/mc_variability_op.cpp")
    w <- diag(W)
    Variability = mc_variability_op(P2 = product2, C = C, WE = WE, k4 = k4, W = w)
    #for (i in 1:n_par) {
    #    for (j in 1:i) {
    #        Variability[i, j] <-
    #            as.numeric(-2 * sensitivity[i, j] +
    #                            sum(k4 * diag(W[[i]]) * diag(W[[j]])))
    #    }
    #}
    Variability <- forceSymmetric(Variability, uplo = FALSE)
    return(Variability)
}
