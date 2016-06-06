#' @title Gosho Information Criterion
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Extract the Gosho Information Criterion (GOSHO)
#' for an object of \code{mcglm} class.
#' WARNING: This function is limited to models with ONE response variable.
#' This function is general useful only for longitudinal data analysis.
#'
#' @param object an object of \code{mcglm} class.
#' @param id a vector which identifies the clusters or groups.
#'     The length and order of id should be the same as the number of
#'     observations. Data are assumed to be sorted so that observations
#'     on a cluster are contiguous rows for all entities in the formula.
#' @param verbose logical. Print or not the GOSHO value.
#' @return The value of the GOSHO criterion.
#' Note that the function assumes that the data are in the correct order.
#'
#' @source Wang, M. (2014). Generalized Estimating Equations in Longitudinal Data
#' Analysis: A Review and Recent Developments. Advances in Statistics, 1(1)1--13.
#'
#' @seealso \code{gof}, \code{plogLik}, \code{pAIC}, \code{pKLIC},
#' \code{ESS} and \code{RJC}.
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
