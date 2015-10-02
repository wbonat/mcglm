#' Fast forward selection for multivariate covariance generalized linear
#' models.
#'
#' @description Perform fast forward model selection using the score
#' information criterion. This function works only for univariate months.
#'
#' @param object an object representing a model of \code{mcglm} class.
#' @param scope a vector specyfing the covariate to be tested.
#' @param interaction Maximum number of covariates interacting.
#' @param penalty penalty term (default = 2).
#' @param n_max Maximum number of models to be fitted.
#' @return The selected model.
#' @export

mc_fast_forward <- function(object, scope, interaction = 1,
                            penalty = 2, n_max = 10) {
  if (interaction > 1) {
    int_terms <- list()
    for (i in 2:interaction) {
      int_terms[[c(i-1)]] <- combn(length(scope), i)
    }
    fun_temp <- function(int_terms) {
      output <- c()
      for(i in 1:dim(int_terms)[2]) {
        output[i] <- paste(scope[int_terms[,i]],collapse = "*")
        }
      return(output)
      }
  scope <- do.call(c, lapply(int_terms, fun_temp))
  }
  for (i in 1:n_max) {
    if(length(scope) == 0) break
    sic <- mc_sic(object = object, scope = scope,
                  data = data, response = 1)
    if (all(sic$SIC < 0)) break
    cov_new <- as.numeric(rownames(sic[which.max(sic$SIC),]))
    cov_enter <- scope[as.numeric(rownames(sic[which.max(sic$SIC),]))]
    next_cov <- paste("~. +", cov_enter)
    new_formula <- update.formula(object$linear_pred[[1]], next_cov)
    temp_models <- mcglm(c(new_formula),
                       matrix_pred = object$matrix_pred,
                       link = object$link, variance = object$variance,
                       covariance = object$covariance, data = data)
    object <- temp_models
    scope <- scope[-cov_new]
    cat(paste("Iteration", i), "\n")
  }
  return(object)
}
