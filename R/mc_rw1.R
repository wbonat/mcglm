#' Random walk first order model
#'
#' @description Builds a random walk first order model matrix.
#'
#' @param n_time Number observations time.
#' @param intrinsic Logical indicating if the models is intrinsic (rho = 1) or not.
#' @return A matrix. Note that the function assumes that the data are in the correct order.
#' @export
mc_rw1 <- function(n_time, intrinsic = TRUE) {
  R <- Matrix(0,nrow = n_time, ncol = n_time, sparse = TRUE)
  ## Border restriction
  ncol = n_time
  R[1,c(1,2)] <- c(1,-1)
  R[ncol,c(ncol-1,ncol)] <- c(-1,1)
  ## Body of matrix
  n <- ncol-1
  for(i in 2:n) {
    R[i,c(i-1,i,i+1)] <- c(-1,2,-1)
  }
  if(intrinsic == TRUE) {output <- list(R)}
  if(intrinsic == FALSE) {
    R1 <- Diagonal(n_time, diag(R))
    diag(R) <- 0
    output <- list("Z1" = R1, "Z2" = R)
  }
  return(output)
}


