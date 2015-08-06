#' Cross variability matrix
#'
#' @description COmpute the cross-covariance matrix between covariance and regression parameters.
#' Equation (11) of Bonat and Jorgensen (2015).
#'
#' @param A A matrix.
#' @param res A vector of residuals.
#' @param W A matrix of weights.
covprod <- function(A, res, W){
  res =as.numeric(res)
  saida <- (res%*%W%*%res)%*%(t(res)%*%A)
  return(as.numeric(saida))
}
