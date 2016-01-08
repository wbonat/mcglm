#' @title Robust standard error for regression parameters
#' @author Wagner Hugo Bonat
#'
#' @description Compute robust standard error for regression parameters
#'     in the context of clustered observations.
#'
#' @param object An object of mcglm class.
#' @param id a vector which identifies the clusters. The length and
#'     order of id should be the same as the number of
#'     observations. Data are assumed to be sorted so that observations
#'     on a cluster are contiguous rows for all entities in the formula.
#' @return A matrix. Note that the function assumes that the data are in
#'     the correct order.
#' @export

mc_robust_std <- function(object, id) {
    inv_M <- object$inv_S_beta
    temp_data <- data.frame(res = object$residuals, id)
    temp_data_group <- split(temp_data, temp_data$id)
    r_rT <- bdiag(lapply(temp_data_group,
                         function(x) {
                             tcrossprod(x[, 1])
                         }))
    D <- bdiag(lapply(object$mu_list, function(x) x$D))
    p1 <- object$inv_C %*% D
    V_robust <- inv_M %*% (t(p1) %*% r_rT %*% p1) %*% inv_M
    output <- sqrt(diag(V_robust))
    return(output)
}
