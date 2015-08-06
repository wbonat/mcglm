#' Pearson estimating function
#'
#' @description Compute the Pearson estimating function its sensitivity and variability matrices.
#'
#' @param y_vec A vector.
#' @param mu_vec A vector.
#' @param A list of matrices.
#' @param inv_J_beta A matrix.
#' @param D A matrix.
#' @param correct Logical.
#' @param compute_variability Logical.
#' @details Compute the Pearson estimating function its sensitivity and variability matrices.
#' For more details see Bonat and Jorgensen (2015) equations \eq{6--8}.
mc_pearson <- function(y_vec, mu_vec, Cfeatures, inv_J_beta = NULL, D = NULL,
                       correct = FALSE, compute_variability = FALSE){
  product <- lapply(Cfeatures$D_C, mc_multiply, bord2 = Cfeatures$inv_C)
  res <- y_vec - mu_vec
  pearson_score <- unlist(lapply(product, mc_core_pearson, inv_C = Cfeatures$inv_C,
                                 res = res))
  sensitivity <- mc_sensitivity(product)
  output <- list("Score" = pearson_score, "Sensitivity" = sensitivity, "Extra" = product)
  if(correct == TRUE){
    correction <- mc_correction(D_C = Cfeatures$D_C, inv_J_beta = inv_J_beta,
                                D = D, inv_C = Cfeatures$inv_C)
    output <- list("Score" = pearson_score + correction, "Sensitivity" = sensitivity,
                   "Extra" = product)
  }
  if(compute_variability == TRUE){
    variability <- mc_variability(sensitivity = sensitivity, product = product,
                                  inv_C = Cfeatures$inv_C, C = Cfeatures$C, res = res)
    output$Variability <- variability
  }
  return(output)
}

