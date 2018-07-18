#' @title Cross variability matrix
#' @author Wagner Hugo Bonat
#'
#' @description Compute the cross-covariance matrix between covariance
#'     and regression parameters.  Equation (11) of Bonat and Jorgensen
#'     (2016).
#'
#' @param A A matrix.
#' @param res A vector of residuals.
#' @param W A matrix of weights.
#' @keywords internal

covprod <- function(A, res, W) {
    res <- as.numeric(res)
    saida <- (res %*% W %*% res) %*% (t(res) %*% A)
    return(as.numeric(saida))
}
