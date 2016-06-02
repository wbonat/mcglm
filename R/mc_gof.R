#' @title Extract Measures of Goodness-of-Fit
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#' @description Extract the plogLik, pAIC, pKLIC and ESS for an object
#' of \code{mcglm} class.
#' @param object an object or a list of objects representing a model
#' of \code{mcglm} class.
#' @return Returns a data frame contains gof's.
#' @seealso \code{plogLik}, \code{pAIC}, \code{pKLIC}, \code{ESS}.
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
