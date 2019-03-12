#' @title Auxiliar function: Build F matrix for Wald multivariate test
#' @name mc_build_F
#' @author Wagner Hugo Bonat
#'
#' @description The function \code{mc_build_F} is just an auxiliar
#'     function to construct the design matrix for multivariate Wald-type
#'     test.
#'
#' @param vector A vector indexing model regression coefficients.
#' @keywords internal
#' @return A matrix.

## Building F matrix for multivariate hypothesis tests ---------------
mc_build_F <- function(vector) {
  FF <- diag(length(vector))
  FF_list <- by(FF, vector, as.matrix)
  return(FF_list)
}
