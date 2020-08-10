#' @title Sensitivity matrix
#' @author Wagner Hugo Bonat and Eduardo Elias Ribeiro Jr
#'
#' @description Compute the sensitivity matrix associated with the
#'     Pearson estimating function.
#'
#' @param product A list of matrix.
#' @param D_C List of matrices.
#' @return The sensitivity matrix associated with the Pearson estimating
#'     function.
#' @keywords internal
#' @details This function implements the equation 7 of Bonat and
#'     Jorgensen (2016).
#' @useDynLib mcglm
#' @importFrom Rcpp sourceCpp

mc_sensitivity <- function(product, D_C) {
    #sourceCpp("src/mc_sensitivity_op.cpp")
    Sensitivity <- mc_sensitivity_op(products = product, D_C = D_C)
    Sensitivity <- forceSymmetric(Sensitivity, uplo = FALSE)
    return(Sensitivity)
}
