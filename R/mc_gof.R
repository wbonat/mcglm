#' @title Extract measures of goodness-of-fit for
#' multivariate covariance generalized linear models.
#' @author Wagner Hugo Bonat
#'
#' @description Extract the plogLik, pAIC, pKLIC and ESS for a fitted McGLM.
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @return Returns a data frame contains gof's.
#' @export

gof <- function(object) {
  pl <- plogLik(object, verbose = FALSE)
  AIC <- pAIC(object, verbose = FALSE)
  KLIC <- pKLIC(object, verbose = FALSE)
  ESS2 <- ESS(object, verbose = FALSE)
  output <- data.frame("plogLik" = pl$plogLik, "Df" = pl$df,
                       "pAIC" = AIC$pAIC,"pKLIC" = KLIC$pKLIC,
                       "ESS" = ESS2)
  return(output)
}
