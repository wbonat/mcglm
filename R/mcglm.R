#' Fitting Multivariate Covariance Generalized Linear Models (McGLM)
#'
#' @description \code{mcglm} is used to fit multivariate covariance generalized linear models.
#' The models are specified by a set of lists giving a symbolic description of the linear predictor.
#' The user can choose between a list of link, variance and covariance functions. The models are
#' fitted using an estimating function approach, combining quasi-score functions for regression
#' parameters and Pearson estimating function for covariance parameters. For details see Bonat and
#' Jorgensen (2015).
#'
#' @param linear_pred A list of formula see \code{\link[stats]{formula}} for details.
#' @param matrix_pred A list of known matrices to be used on the matrix linear predictor. Details
#' can be obtained on \code{\link[mcglm]{mc_matrix_linear_predictor}}.
#' @param link A list of link functions names, see \code{\link[mcglm]{mc_link_function}} for details.
#' @param variance A list of variance functions names, see \code{\link[mcglm]{mc_variance_function}}
#' for details.
#' @param covariance A list of covariance link functions names, current options are: identity, inverse
#' and exponential-matrix (expm).
#' @param offset A list with values of offset values if any.
#' @param Ntrial A list with values of the number of trials on Bernoulli experiments. It is useful only
#' for binomialP and binomialPQ variance functions.
#' @param power_fixed A list of logicals indicating if the values of the power parameter should be
#' estimated or not.
#' @param control_initial A list of initial values for the fitting algorithm. See details below.
#' @param control_algorithm A list of arguments to be passed for the fitting algorithm. See
#' \code{\link[mcglm]{fit_mcglm}} for details.
#' @return mcglm returns an object of class "mcglm".
#' @export
mcglm <- function(linear_pred, matrix_pred,
                  link, variance, covariance,
                  offset, Ntrial, power_fixed,
                  data,
                  control_initial,
                  control_algorithm = list()) {
  con <- list("correct" = TRUE, "max_iter" = 20, "tol" = 1e-03,
       "method" = "chaser", "tunning" = 1, "verbose" = TRUE)
  con[(namc <- names(control_algorithm))] <- control_algorithm
  list_X <- lapply(linear_pred, model.matrix, data = data)
  list_model_frame <- lapply(linear_pred, model.frame, data = data)
  list_Y <- lapply(list_model_frame, model.response)
  y_vec <- as.numeric(do.call(c, list_Y))
  sparse <- lapply(matrix_pred, function(x) {
    if(class(x) == "dgeMatrix"){FALSE} else TRUE})
  model_fit <- fit_mcglm(list_initial = control_initial,
                         list_link = link, list_variance = variance, list_covariance = covariance,
                         list_X = list_X, list_Z = matrix_pred, list_offset = offset,
                         list_Ntrial = Ntrial, list_power_fixed = power_fixed,
                         list_sparse = sparse, y_vec = y_vec,
                         correct = con$correct,
                         max_iter = con$max_iter,
                         tol = con$tol,
                         method = con$method,
                         tunning = con$tunning,
                         verbose = con$verbose)
  model_fit$beta_names <- lapply(list_X, colnames)
  model_fit$power_fixed <- power_fixed
  model_fit$list_initial <- list_initial
  model_fit$n_obs <- dim(data)[1]
  model_fit$link <- link
  model_fit$variance <- variance
  model_fit$covariance <- covariance
  model_fit$linear_pred <- linear_pred
  model_fit$con <- con
  model_fit$observed <- Matrix(y_vec, ncol = length(list_Y), nrow = dim(data)[1])
  model_fit$list_X <- list_X
  class(model_fit) <- "mcglm"
  return(model_fit)
}
