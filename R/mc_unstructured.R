#' Unstructured model
#'
#' @description Builds a unstructured model matrix.
#'
#' @param n_time Number of observations per unit sample.
#' @return A matrix. Note that the function assumes that the data are in the correct order.
#' @export

mc_unstructured <- function(n_time) {
    mat.temp <- Matrix(0, ncol = n_time, nrow = n_time, sparse = TRUE)
    non.diagonal.terms <- list()
    non.diagonal <- t(combn(n_time, 2))
    n.cor.par <- dim(non.diagonal)[1]
    ## Covariance elementary matrices
    for (i in 1:n.cor.par) {
        non.diagonal.terms[i][[1]] <- mat.temp
        non.diagonal.terms[i][[1]][non.diagonal[i, 1], non.diagonal[i, 2]] <- non.diagonal.terms[i][[1]][non.diagonal[i,
            2], non.diagonal[i, 1]] <- 1
    }
    ## Output
    return(non.diagonal.terms)
}
