#' @title Exponential-matrix and its derivatives
#' @author Wagner Hugo Bonat
#'
#' @description Given a matrix \eqn{M} and its derivative \eqn{dM} the
#'     function \code{dexp_gold} returns the exponential-matrix
#'     \eqn{expm(M)} and its derivative. This function is based on the
#'     \code{\link[Matrix]{expm}} function. It is not really used in the
#'     package, but I keep this function to test my own implementation
#'     based on eigen values decomposition.
#'
#' @param M A matrix.
#' @param dM A matrix.
#' @keywords internal
#' @return A list with two elements: \eqn{expm(M)} and its derivatives.
#' @seealso \code{\link[Matrix]{expm}}, \code{\link[base]{eigen}}.
#' @examples
#' M <- matrix(c(1,0.8,0.8,1), 2,2)
#' dM <- matrix(c(0,1,1,0),2,2)
#' mcglm::mc_dexp_gold(M = M, dM = dM)
#' @export

mc_dexp_gold <- function(M, dM) {
    N <- dim(M)
    AM <- rbind(cbind(M, matrix(0, N[1], N[2])), cbind(dM, M))
    PSI <- expm(AM)
    F <- PSI[1:N[1], 1:N[1]]
    dF <- PSI[c(N[1] + 1):c(2 * N[1]), 1:N[1]]
    return(list(F, dF))
}
