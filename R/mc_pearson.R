#' @title Pearson estimating function
#' @author Wagner Hugo Bonat
#'
#' @description Compute the Pearson estimating function its sensitivity
#'     and variability matrices.
#'
#' @param y_vec A vector.
#' @param mu_vec A vector.
#' @param Cfeatures A list of matrices.
#' @param inv_J_beta A matrix.
#' @param D A matrix.
#' @param correct Logical.
#' @param compute_variability Logical.
#' @param compute_sensitivity Logical.
#' @return A list with three components: (i) a vector of quasi-score
#'     values, (ii) the sensitivity and (iii) variability matrices
#'     associated with the Pearson estimating function.
#' @keywords internal
#' @details Compute the Pearson estimating function its sensitivity and
#'     variability matrices.  For more details see Bonat and Jorgensen
#'     (2016) equations 6, 7 and 8.

mc_pearson <- function(y_vec, mu_vec, Cfeatures, inv_J_beta = NULL,
                       D = NULL, correct = FALSE,
                       compute_sensitivity = TRUE,
                       compute_variability = FALSE) {
    product <- lapply(Cfeatures$D_C, mc_multiply,
                      bord2 = Cfeatures$inv_C)
    res <- y_vec - mu_vec
    pearson_score <- unlist(lapply(product, mc_core_pearson,
                                   inv_C = Cfeatures$inv_C, res = res))
    sensitivity <- matrix(NA, length(product), length(product))
    if(compute_sensitivity == TRUE) {
      sensitivity <- mc_sensitivity(product)
    }

    output <- list(Score = pearson_score, Sensitivity = sensitivity,
                   Extra = product)
    if (correct == TRUE) {
        correction <- mc_correction(D_C = Cfeatures$D_C,
                                    inv_J_beta = inv_J_beta, D = D,
                                    inv_C = Cfeatures$inv_C)
        output <- list(Score = pearson_score + correction,
                       Sensitivity = sensitivity, Extra = product)
    }
    if (compute_variability == TRUE) {
        variability <- mc_variability(sensitivity = sensitivity,
                                      product = product,
                                      inv_C = Cfeatures$inv_C,
                                      C = Cfeatures$C, res = res)
        output$Variability <- variability
    }
    return(output)
}
