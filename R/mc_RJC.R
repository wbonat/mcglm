#' @title Rotnitzky-Jewell’s information criterion
#' @author Wagner Hugo Bonat
#'
#' @description Compute the Rotnitzky-Jewell’s information criterion
#' for multivariate covariance generalized linear models.
#' WARNINGS: This function is limited to models with ONE response variable.
#'
#' @param object An object of mcglm class.
#' @param id a vector which identifies the clusters. The length and
#'     order of id should be the same as the number of
#'     observations. Data are assumed to be sorted so that observations
#'     on a cluster are contiguous rows for all entities in the formula.
#' @return A matrix. Note that the function assumes that the data are in
#'     the correct order.
#' @export

RJC <- function(object, id, verbose = TRUE) {
  temp_data <- data.frame(res = object$residuals, id)
  temp_data_group <- split(temp_data, temp_data$id)
  r_rT <- bdiag(lapply(temp_data_group,
                       function(x) {
                         tcrossprod(x[, 1])
                       }))
  D <- bdiag(lapply(object$mu_list, function(x) x$D))
  p1 <- t(D)%*%object$inv_C
  Omega0 <- p1%*%r_rT%*%t(p1)
  Omega1 <- p1%*%D
  Omega <- solve(Omega0)%*%Omega1
  df <- dim(Omega)[1]
  t1 <- (1 - sum(diag(Omega))/df)^2
  t2 <- (1 - sum(diag(Omega^2))/df)^2
  RJC <- sqrt(t1 + t2)
  if (verbose) cat("RJC", RJC)
  return(invisible(list("RJC" = RJC)))
}
