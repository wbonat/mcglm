#' Confidence Intervals for mcglm
#'
#' @description Computes confidence intervals for parameters in a fitted mcglm.
#'
#' @param object a fitted mcglm object.
#' @param level the confidence level required.
#' @return A data.frame with confidence intervals, parameters names,
#' response number and parameters type.
#' @export
confint.mcglm <- function(object, level = 0.95) {
  temp <- coef(object,std.error = TRUE)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  ci <- temp$Estimates + temp$Std.error%o%fac
  colnames(ci) <- paste(format(a,2),"%", sep = "")
  rownames(ci) <- temp$Parameters
  return(ci)
}
