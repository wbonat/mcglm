#' Random walk second order model
#'
#' @description Builds a random walk second order model matrix.
#'
#' @param n_time Number observations time.
#' @param intrinsic Logical indicating if the models is intrinsic (rho = 1) or not.
#' @return A matrix. Note that the function assumes that the data are in the correct order.
#' @export
mc_rw2 <- function(n_time, intrinsic = TRUE) {
    R <- Matrix(0, nrow = n_time, ncol = n_time, sparse = TRUE)
    ## Border restriction
    ncol = n_time
    R[1, c(1, 2, 3)] <- c(1, -2, 1)
    R[ncol, c(c(ncol - 2):ncol)] <- c(1, -2, 1)
    R[2, c(1:4)] <- c(-2, 5, -4, 1)
    R[c(ncol - 1), c(c(ncol - 3):c(ncol))] <- c(1, -4, 5, -2)
    ## Body of matrix
    n <- ncol - 2
    for (i in 3:n) {
        R[i, c(i - 2, i - 1, i, i + 1, i + 2)] <- c(1, -4, 6, -4, 1)
    }
    if (intrinsic == TRUE) {
        output <- list(R)
    }
    if (intrinsic == FALSE) {
        R1 <- Diagonal(n_time, diag(R))
        diag(R) <- 0
        output <- list(Z1 = R1, Z2 = R)
    }
    return(output)
} 
