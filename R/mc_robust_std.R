#' @title Robust Standard Error for Regression Parameters
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Compute robust standard error for regression parameters
#'     in the context of clustered observations for an object of
#'     \code{mcglm} class.
#'
#' @param object an object of \code{mcglm} class.
#' @param id a vector which identifies the clusters or subject indexes.
#'     The length and order of \code{id} should be the same as the number of
#'     observations. Data are assumed to be sorted so that observations
#'     on a cluster are contiguous rows for all entities in the formula.
#'
#' @return A variance-covariance matrix.
#'     Note that the function assumes that the data are in the correct
#'     order.
#'
#' @source Nuamah, I. F. and Qu, Y. and Aminu, S. B. (1996). A SAS macro
#' for stepwise correlated binary regression. Computer Methods
#' and Programs in Biomedicine 49, 199--210.
#'
#' @seealso \code{mc_bias_correct_std}.
#'
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
    output <- list("Std.Error" = sqrt(diag(V_robust)),
                                      "vcov" = V_robust)
    return(output)
}
