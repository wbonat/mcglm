#' @title Sensitivity matrix
#' @author Wagner Hugo Bonat
#'
#' @description Compute the sensitivity matrix associated with the
#'     Pearson estimating function.
#'
#' @param product A list of matrix.
#' @return The sensitivity matrix associated with the Pearson estimating
#'     function.
#' @keywords internal
#' @details This function implements the equation 7 of Bonat and
#'     Jorgensen (2016).

mc_sensitivity <- function(product) {
    n_par <- length(product)
    Sensitivity <- matrix(0, n_par, n_par)
    Sensitivity_temp <- matrix(0, n_par, n_par)
    Sensitivity1 <- matrix(0, n_par, n_par)
    for (i in 1:n_par) {
        for (j in 1:n_par) {
            Sensitivity[i, j] <- -sum(t(product[[i]]) * product[[j]])
        }
    }
    return(Sensitivity)
}
