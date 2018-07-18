#' @title Conditional Hypotheses Tests
#'
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Compute conditional hypotheses tests for fitted
#' \code{mcglm} model class.
#' When fitting models with extra power parameters, the standard errors
#' associated with the dispersion parameters can be large. In that cases,
#' we suggest to conduct conditional hypotheses test instead of the
#' orthodox marginal test for the dispersion parameters.
#' The function \code{mc_conditional_test} offers an ease way to
#' conduct such conditional test. Furthermore, the function is quite
#' flexible and can be used for any other conditional hypotheses test.
#'
#' @param object an object representing a model of \code{mcglm} class.
#' @param parameters which parameters will be included in the
#' conditional test.
#' @param test index indicating which parameters will be tested given
#' the values of the other parameters.
#' @param fixed index indicating which parameters should be fixed on
#' the conditional test.
#'
#' @return Returns estimates, conditional standard errors and Z-statistics.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @export

mc_conditional_test <- function(object, parameters, test, fixed) {
  Sigma <- vcov(object)[parameters,parameters]
  Sigma11 <- Sigma[test,test]
  Sigma12 <- Sigma[test,fixed]
  Sigma21 <- Sigma[fixed,test]
  Sigma22 <- Sigma[fixed,fixed]
  Sigma.cond <- Sigma11 - Sigma12%*%solve(Sigma22)%*%Sigma21
  std.error.cond <- sqrt(diag(Sigma.cond))
  point <- coef(object)$Estimates[coef(object)$Parameters %in% parameters[test]]
  output <- data.frame("Estimates" = point,
                       "Std.error" = std.error.cond,
                       "Z.value" = point/std.error.cond)
  return(output)
}
