#' Independent model structure
#'
#' @description Builds a identity matrix to be used as a component of
#' the matrix linear predictor.
#'
#' @param data The data set to be used.
#' @return A list of matrix.
#' @export

mc_id <- function(data) {
  output <- list("Z0" = Diagonal(dim(data)[1],1))
  return(output)
}
