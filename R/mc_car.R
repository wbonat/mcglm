#' Conditional autoregressive models
#'
#' @description Builds conditional autoregressive model matrix.
#'
#' @param list_neigh List of neighboors.
#' @param intrinsic Logical
#' @return A matrix.
#' @export


mc_car <- function(list_neigh, intrinsic = FALSE) {
  nrow = ncol = length(list_neigh)
  W <- Matrix(0, nrow = nrow, ncol = ncol, sparse = TRUE)
  diagonal <- c()
  for(i in 1:ncol) {
    diagonal[i] <- length(list_neigh[[i]])
    W[i,c(list_neigh[[i]])] <- -1
    W <- forceSymmetric(W)
  }
  if(intrinsic == TRUE) {
    diag(W) <- diagonal
    output <- list(W)
  }
  if(intrinsic == FALSE) {
    D <- Diagonal(nrow, diagonal)
    output <- list(D, W)
  }
  return(output)
}
