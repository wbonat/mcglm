#' @title Core of the Pearson estimating function.
#' @author Wagner Hugo Bonat
#'
#' @description Core of the Pearson estimating function.
#'
#' @param product A matrix.
#' @param C A matrix.
#' @param res A vector of residuals.
#' @param W Matrix of weights.
#' @return A vector
#' @keywords internal
#' @details It is an internal function.

mc_core_pearson <- function(product, C, res, W) {
    output <- t(res) %*% product %*% res - sum(diag(product %*% C))
    return(as.numeric(output))
}
