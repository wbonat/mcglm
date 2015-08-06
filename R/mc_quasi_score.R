#' Quasi-score function
#'
#' @description Compute the quasi-score function, its sensitivy and variability matrix.
#'
#' @param D A matrix. In general the output from \code{\link[mcglm]{mc_lnk_function}}.
#' @param inv_C A matrix. In genral the output from \code{\link[mcglm]{mc_build_C}}.
#' @param y_vec A vector.
#' @param mu_vec A vector.
#' @return The quasi-score vector, the Sensivity and variability matrices.

mc_quasi_score <- function(D, inv_C, y_vec, mu_vec){
  res <- y_vec - mu_vec
  t_D <- t(D)
  score <-t_D%*%(inv_C%*%res)
  sensitivity <- -t_D%*%inv_C%*%D
  output <- list("Score" = score, "Sensitivity" = sensitivity, "Variability" = -sensitivity)
  return(output)
}



