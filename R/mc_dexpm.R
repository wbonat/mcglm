#' @title Exponential-matrix covariance link function
#' @author Wagner Hugo Bonat
#'
#' @description Given a matrix \code{U} the function \code{mc_expm}
#'     returns the exponential-matrix \eqn{expm(U)} and some auxiliares
#'     matrices to compute its derivatives. This function is based on
#'     the eigen-value decomposition it means that it is very slow.
#'
#' @param U A matrix.
#' @param n A number specifing the dimension of the matrix U. Default
#'     \code{n = dim(U)[1]}.
#' @param sparse Logical defining the class of the output matrix. If
#'     \code{sparse = TRUE} the output class will be 'dgCMatrix' if
#'     \code{sparse = FALSE} the class will be 'dgMatrix'.
#' @param inverse Logical defining if the inverse will be computed or
#'     not.
#' @return A list with \eqn{\Omega = expm(U)} its inverse (if
#'     \code{inverse = TRUE}) and auxiliares matrices to compute the
#'     derivatives.
#' @keywords internal
#' @seealso \code{\link[Matrix]{expm}}, \code{\link[base]{eigen}},
#'     \code{link[mcglm]{mc_dexp_gold}}.

mc_expm <- function(U, n = dim(U)[1], sparse = FALSE, inverse = FALSE) {
    tt <- eigen(U, symmetric = TRUE)
    UU <- tt$vectors
    Q <- tt$values
    eQr <- Diagonal(n, exp(tt$values))
    inv_UU <- t(UU)
    Omega <- Matrix(UU %*% eQr %*% inv_UU, sparse = sparse)
    if (inverse == TRUE) {
        eQr_INV <- Diagonal(n, exp(-tt$values))
        inv_Omega <- Matrix(UU %*% eQr_INV %*% inv_UU, sparse = sparse)
        saida <- list(Omega = Omega, inv_Omega = inv_Omega, UU = UU,
                      Q = Q, inv_UU = inv_UU)
    }
    if (inverse == FALSE) {
        saida <- list(Omega = Omega, UU = UU, Q = Q, inv_UU = inv_UU)
    }
    return(saida)
}
