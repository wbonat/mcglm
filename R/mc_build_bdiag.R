#'@title Build a block-diagonal matrix of zeros.
#'@name mc_build_bdiag
#'@author Wagner Hugo Bonat
#'
#'@description Build a block-diagonal matrix of zeros. Such functions
#'     is used when computing the derivatives of the Cholesky
#'     decomposition of C.
#'
#'@param n_resp A numeric specifyng the number of response variables.
#'@param n_obs A numeric specifying the number of observations in the
#'     data set.
#'@keywords internal
#'@return A list of zero matrices.
#'@details It is an internal function.

mc_build_bdiag <- function(n_resp, n_obs) {
    list_zero <- list()
    for (i in 1:n_resp) {
        list_zero[[i]] <- Matrix(0, n_obs, n_obs, sparse = TRUE)
    }
    return(list_zero)
}
