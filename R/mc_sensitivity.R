#' @title Sensitivity matrix
#' @author Wagner Hugo Bonat and Eduardo Elias Ribeiro Jr
#'
#' @description Compute the sensitivity matrix associated with the
#'     Pearson estimating function.
#'
#' @param product A list of matrix.
#' @param W Matrix of weights.
#' @return The sensitivity matrix associated with the Pearson estimating
#'     function.
#' @keywords internal
#' @details This function implements the equation 7 of Bonat and
#'     Jorgensen (2016).
#' @useDynLib mcglm
#' @importFrom Rcpp sourceCpp

mc_sensitivity <- function(product, W) {
    #sourceCpp("src/mc_sensitivity_op.cpp")
    Sensitivity <- mc_sensitivity_op(products = product, W = W)
    Sensitivity <- forceSymmetric(Sensitivity, uplo = "L")
    return(Sensitivity)
}
