#' @title Remove NA from Matrix Linear Predictor
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_remove_na} removes NA from each
#' component of the matrix linear predictor. It is in general used after
#' the function \code{mc_complete_data}.
#'
#' @param matrix_pred a list of known matrices.
#' @param cod index indicating the columns should be removed.
#' @return A list of matrices.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @seealso \code{mc_dglm}, \code{mc_ns}, \code{mc_ma} and \code{mc_rw}.
#' @export

mc_remove_na <- function(matrix_pred, cod) {
  matrix_pred_temp <- list()
  for(i in 1:length(matrix_pred)) {
    matrix_pred_temp[[i]] <- matrix_pred[[i]][-cod,-cod]
  }
  return(matrix_pred_temp)
}
