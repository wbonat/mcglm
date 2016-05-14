#' @title Matrix product in sandwich form
#' @name mc_sandwich
#' @author Wagner Hugo Bonat
#'
#' @description The function \code{mc_sandwich} is just an auxiliar
#'     function to compute product matrix in the sandwich form
#'     \code{bord1 * middle * bord2}. An special case appears when
#'     computing the derivative of the covariance matrix with respect to
#'     the power parameter. Always the bord1 and bord2 should be
#'     diagonal matrix. If it is not true, this product is too slow.
#'
#' @param middle A matrix.
#' @param bord1 A matrix.
#' @param bord2 A matrix.
#' @keywords internal
#' @return The matrix product \code{bord1 * middle * bord2}.

## Auxiliar function to multiply matrices ------------------------------
mc_sandwich <- function(middle, bord1, bord2) {
    bord1 %*% middle %*% bord2
}

#' @rdname mc_sandwich
mc_sandwich_negative <- function(middle, bord1, bord2) {
    -bord1 %*% middle %*% bord2
}

#' @rdname mc_sandwich
mc_sandwich_power <- function(middle, bord1, bord2) {
    temp1 <- mc_sandwich(middle = middle, bord1 = bord1, bord2 = bord2)
    return(temp1 + t(temp1))
}

#' @rdname mc_sandwich
mc_sandwich_cholesky <- function(bord1, middle, bord2) {
    p1 <- bord1 %*% middle %*% bord2
    return(p1 + t(p1))
}

#' @rdname mc_sandwich
mc_multiply <- function(bord1, bord2) {
    return(bord2 %*% bord1)
}

#' @rdname mc_sandwich
mc_multiply2 <- function(bord1, bord2) {
    return(bord1 %*% bord2)
}
