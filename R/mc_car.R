#' @title Conditional Autoregressive Model Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_car} helps to build the components
#' of the matrix linear predictor used for fitting conditional
#' autoregressive models. This function is used in general for fitting
#' spatial areal data using the well known conditional autoregressive
#' models (CAR). This function depends on a list of neighboors, such a
#' list can be constructed, for example using the
#' \code{tri2nb} function from the \code{spdep} package
#' based on spatial coordinates. This way to specify the matrix linear
#' predictor can also be applied for spatial continuous data,
#' as an approximation.
#'
#' @param list_neigh list of neighboors.
#' @param intrinsic logical.
#' @return A list of a matrix (\code{intrinsic = TRUE}) or two matrices
#' (\code{intrinsic = FALSE}).
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @seealso \code{mc_id}, \code{mc_compute_rho}, \code{mc_conditional_test},
#'  \code{mc_dist}, \code{mc_ma}, \code{mc_rw} \cr and \code{mc_mixed}.
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
