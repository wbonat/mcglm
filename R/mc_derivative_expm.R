#' @title Derivative of exponential-matrix function
#' @author Wagner Hugo Bonat
#'
#' @description Compute the derivative of the exponential-matrix
#'     covariance link function.
#'
#' @param UU A matrix.
#' @param inv_UU A matrix.
#' @param Q A numeric vector.
#' @param dU A matrix.
#' @param n A numeric.
#' @param sparse Logical.
#' @return A matrix.
#' @seealso \code{\link[Matrix]{expm}}, \code{link[mcglm]{mc_dexp_gold}}
#'     and \code{link[mcglm]{mc_dexpm}}.
#' @keywords internal
#' @details Many arguments required by this function are provide by the
#'     \code{link[mcglm]{mc_dexpm}}.  The argument dU is the derivative
#'     of the U matrix with respect to the models parameters. It should
#'     be computed by the user.

mc_derivative_expm <- function(dU, UU, inv_UU, Q, n = dim(UU)[1],
                               sparse = FALSE) {
    H <- inv_UU %*% dU %*% UU
    P <- Matrix(0, ncol = n, nrow = n)
    diag(P) <- diag(H) * exp(Q)
    P[upper.tri(P)] <- H[upper.tri(H)] * c(dist(exp(Q))/dist(Q))
    P[is.na(P)] <- 0
    P <- forceSymmetric(P)
    D_Omega <- Matrix(UU %*% P %*% inv_UU, sparse = FALSE)
    return(D_Omega)
}
