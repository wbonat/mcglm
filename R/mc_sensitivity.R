#' Sensitivity matrix
#'
#' @description Compute the sensitivity matrix associated with the Pearson estimating function.
#'
#' @param product A list of matrix.
#' @return The sensitivity matrix associated with the Pearson estimating function.
#' @details This function implements the equation 7 of Bonat and Jorgensen (2015).

mc_sensitivity <- function(product) {
    n_par <- length(product)
    Sensitivity <- matrix(0, n_par, n_par)
    Sensitivity_temp <- matrix(0, n_par, n_par)
    Sensitivity1 <- matrix(0, n_par, n_par)
    for (i in 1:n_par) {
        for (j in 1:n_par) {
            # Sensitivity_temp[i,j] <- -sum(diag(product[[i]]%*%product[[j]]))
            Sensitivity[i, j] <- -sum(t(product[[i]]) * product[[j]])
            # Sensitivity1[i,j] <- -sum(product[[i]]*product[[j]])
        }
    }
    # print(forceSymmetric(Sensitivity)) print(forceSymmetric(Sensitivity_temp)) print(forceSymmetric(Sensitivity1))
    # print(all.equal(Sensitivity1, Sensitivity))
    return(Sensitivity)
}
