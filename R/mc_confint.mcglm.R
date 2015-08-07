#' Confidence Intervals for mcglm
#'
#' @description Computes confidence intervals for parameters in a fitted mcglm.
#'
#' @param object a fitted mcglm object.
#' @param level the confidence level required.
#' @return A data.frame with confidence intervals, parameters names, response number and parameters type.

confint.mcglm <- function(object, level = 0.95){
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
  parameters <- c(object$Regression,object$Covariance)
  std.errors <- sqrt(diag(object$vcov))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  ci <- parameters + std.errors%o%fac
  colnames(ci) <- paste(format(a,2),"%", sep = "")
  rownames(ci) <- cod
  return(ci)
}
