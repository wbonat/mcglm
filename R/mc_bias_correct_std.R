#' Bias-corrected standard error for regression parameters
#'
#' @description Compute bias-corrected standard error for regression parameters in the context
#' of clustered observations. It is also robust and has improved finite sample properties.
#'
#' @param object An object of mcglm class.
#' @param id a vector which identifies the clusters. The length and order of id should be the
#' same as the number of observations. Data are assumed to be sorted so that observations on a cluster
#' are contiguous rows for all entities in the formula.
#' @return A matrix. Note that the function assumes that the data are in the correct order.
#' @export

mc_bias_corrected_std <- function(object, id) {
    inv_M <- object$inv_S_beta
    temp_data <- data.frame(res = object$residuals, id)
    temp_data_group <- split(temp_data, temp_data$id)
    D <- bdiag(lapply(object$mu_list, function(x) x$D))
    r_rT <- bdiag(lapply(temp_data_group, function(x) {
        tcrossprod(x[, 1])
    }))
    tD_invC <- t(D) %*% object$inv_C
    H <- Matrix(D %*% inv_M %*% tD_invC, sparse = TRUE)
    IH <- Diagonal(object$n_obs, 1) - H
    inv_IH <- solve(IH)
    output <- sqrt(diag(inv_M %*% tD_invC %*% inv_IH %*% r_rT %*% inv_IH %*% t(tD_invC) %*% inv_M))
    return(output)
}
