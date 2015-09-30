#' Derivatives of the Cholesky decomposition
#' @description This function compute the derivative of the Cholesky decomposition.
#'
#' @param derivada A matrix.
#' @param inv_chol_Sigma A matrix.
#' @param chol_Sigma A matrix.
#' @return A list of matrix.
#' @details It is an internal function.

mc_derivative_cholesky <- function(derivada, inv_chol_Sigma, chol_Sigma) {
    faux <- function(derivada, inv_chol_Sigma, chol_Sigma) {
        t1 <- inv_chol_Sigma %*% derivada %*% t(inv_chol_Sigma)
        t1 <- tril(t1)
        diag(t1) <- diag(t1)/2
        output <- chol_Sigma %*% t1
        return(output)
    }
    list_D_chol <- lapply(derivada, faux, inv_chol_Sigma = inv_chol_Sigma, chol_Sigma = chol_Sigma)
    return(list_D_chol)
}
