#' @title Compute conditional hypotheses test for multivariate covariance
#' generalized linear models
#' @author Wagner Hugo Bonat
#'
#' @description Compute conditional hypotheses test for fitted McGLMs.
#' It is useful in general when estimating the power parameter.
#' @param object an object representing a model of \code{mcglm} class.
#' @param parameters Which parameters have been included in the
#' conditional test.
#' @param test Index indicating which parameters will be tested given
#' the values of the other parameters.
#' @param fixed Index indicating which parameters should be fixed on
#' the conditional test.
#' @return Returns estimates, conditional standard errors and Z statistics.
#' @export

mc_conditional_test <- function(object, parameters, test, fixed) {
  Sigma <- vcov(object)[parameters,parameters]
  Sigma11 <- Sigma[test,test]
  Sigma12 <- Sigma[test,fixed]
  Sigma21 <- Sigma[fixed,test]
  Sigma22 <- Sigma[fixed,fixed]
  Sigma.cond <- Sigma11 - Sigma12%*%solve(Sigma22)%*%Sigma21
  std.error.cond <- sqrt(diag(Sigma.cond))
  point <- coef(object)$Estimates[coef(object)$Parameters == parameters[test]]
  output <- data.frame("Estimates" = point,
                       "Std.error" = std.error.cond,
                       "Z.value" = point/std.error.cond)
  return(output)
}
