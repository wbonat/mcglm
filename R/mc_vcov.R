#' Calculate Variance-Covariance matrix for a fitted McGLM object.
#'
#' @description Returns the variance-covariance matrix for all parameters of a mcglm fitted model object.
#'
#' @param object a fitted model mcglm object.
#' @return A variance-covariance matrix.

vcov.mcglm <- function(object){
  n_resp <- length(object$beta_names)
  cod_beta <- list()
  cod_power <- list()
  cod_tau <- list()
  for(i in 1:n_resp){
    cod_beta[[i]] <- paste(paste("beta", i, sep = ""), 0:c(object$Information$n_betas[[i]]-1), sep = "")
    cod_power[[i]] <- paste(paste("power", i, sep = ""), 1:object$Information$n_power[[i]], sep = "")
    cod_tau[[i]] <- paste(paste("tau", i, sep = ""), 1:object$Information$n_tau[[i]], sep = "")
  }
  rho_names <- c()
  if(n_resp != 1){
    combination <- combn(n_resp,2)
    for(i in 1:dim(combination)[2]){
      rho_names[i] <- paste(paste("rho", combination[1,i], sep = ""), combination[2,i], sep = "")
    }
  }
  cod <- c(do.call(c,cod_beta),rho_names,
           do.call(c,Map(c,cod_power, cod_tau)))
  colnames(object$vcov) <- cod
  rownames(object$vcov) <- cod
  return(object$vcov)
}
