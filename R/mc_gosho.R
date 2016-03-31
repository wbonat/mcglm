#' @title Gosho’s information criterion
#' @author Wagner Hugo Bonat
#'
#' @description Compute the Gosho’s information criterion
#' for multivariate covariance generalized linear models.
#' WARNINGS: This function is limited to models with ONE response variable.
#'
#' @param object An object of mcglm class.
#' @param id a vector which identifies the clusters. The length and
#'     order of id should be the same as the number of
#'     observations. Data are assumed to be sorted so that observations
#'     on a cluster are contiguous rows for all entities in the formula.
#' @param verbose Logical.
#' @return A matrix. Note that the function assumes that the data are in
#'     the correct order.
#' @export

GOSHO <- function(object, id, verbose = TRUE) {
  temp_data <- data.frame(res = object$residuals, id)
  temp_data_group <- split(temp_data, temp_data$id)
  r_rT <- bdiag(lapply(temp_data_group,
                       function(x) {
                         tcrossprod(x[, 1])
                       }))
  II <- Diagonal(dim(r_rT)[1], 1)
  G <- length(unique(id))
  Gosho <- sum(diag((r_rT%*%object$inv_C - II)^2))/G
  if (verbose) cat("Gosho", Gosho)
  return(invisible(list("Gosho" = Gosho)))
}