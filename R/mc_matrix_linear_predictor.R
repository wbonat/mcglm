#' @title Matrix Linear Predictor
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Compute the matrix linear predictor. It is an internal
#' function, however, since the concept of matrix linear predictor
#' was proposed recently. I decided let this function visible to the
#' interested reader gets some feeling about how it works.
#'
#' @param tau a numeric vector of dispersion parameters.
#' @param Z a list of known matrices.
#'
#' @return A matrix.
#'
#' @export
#' @import Matrix
#'
#' @details Given a list with a set of known matrices
#'     (\eqn{Z_0,...,Z_D}) the function \cr
#'     \code{mc_matrix_linear_predictor} returns \eqn{U = \tau_0 Z_0 +
#'     ... + \tau_D Z_D}.
#'
#' @seealso \code{mc_id}, \code{mc_dist}, \code{mc_ma}, \code{mc_rw},
#' \code{mc_mixed} and \code{mc_car}.
#'
#' @source Bonat, W. H. and Jorgensen, B. (2016) Multivariate
#'     covariance generalized linear models.
#'     Journal of Royal Statistical Society - Series C X(X):XX--XX.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @examples
#' require(Matrix)
#' Z0 <- Diagonal(5, 1)
#' Z1 <- Matrix(rep(1,5)%*%t(rep(1,5)))
#' Z <- list(Z0, Z1)
#' mc_matrix_linear_predictor(tau = c(1,0.8), Z = Z)

# Matrix linear predictor ----------------------------------------------
mc_matrix_linear_predictor <- function(tau, Z) {
    if (length(Z) != length(tau)) {
        stop("Incorrect number of parameters")
    }
    output <- mapply("*", Z, tau, SIMPLIFY = FALSE)
    output <- Reduce("+", output)
    return(output)
}
