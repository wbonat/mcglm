#' @title Quasi-score function
#' @author Wagner Hugo Bonat
#'
#' @description Compute the quasi-score function, its sensitivy and
#'     variability matrix.
#'
#' @param D A matrix. In general the output from
#'     \code{\link[mcglm]{mc_link_function}}.
#' @param inv_C A matrix. In general the output from
#'     \code{\link[mcglm]{mc_build_C}}.
#' @param y_vec A vector.
#' @param mu_vec A vector.
#' @param W Matrix of weights.
#' @keywords internal
#' @return The quasi-score vector, the Sensivity and variability
#'     matrices.
#' @export

mc_quasi_score <- function(D, inv_C, y_vec, mu_vec, W) {
    res <- y_vec - mu_vec
    t_D <- t(D)
    part1 <- t_D %*% inv_C
    score <- part1 %*% W %*% res
    sensitivity <- -part1 %*% W %*% D
    variability <- part1 %*% W^2 %*% D
    output <- list(Score = score, Sensitivity = sensitivity,
                   Variability = variability)
    return(output)
}
