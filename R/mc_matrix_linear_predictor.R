#' Matrix linear predictor
#'
#' @description Compute the matrix linear predictor.
#'
#' @param tau A numeric vector.
#' @param Z   A list of known matrices.
#' @return A matrix.
#' @export
#' @import Matrix
#' @details Given a list with a set of known matrices (\eqn{Z_0,...,Z_D}) the function
#' \code{mc_matrix_linear_predictor} returns \eqn{U = \tau_0 Z_0 + ... + \tau_D Z_D}.
#' @examples
#' require(Matrix)
#' Z0 <- Diagonal(5, 1)
#' Z1 <- Matrix(rep(1,5)%*%t(rep(1,5)))
#' Z <- list(Z0, Z1)
#' mc_matrix_linear_predictor(tau = c(1,0.8), Z = Z)

# Matrix linear predictor -------------------------------
mc_matrix_linear_predictor <- function(tau, Z) {
    if (length(Z) != length(tau)) {
        stop("Incorrect number of parameters")
    }
    output <- mapply("*", Z, tau, SIMPLIFY = FALSE)  ## Multiply each matrix by the parameter tau
    output <- Reduce("+", output)  ## Sum all matrices inside the list
    return(output)
}
