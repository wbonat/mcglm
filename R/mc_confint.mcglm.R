#' Confidence Intervals for mcglm
#'
#' @description Computes confidence intervals for parameters in a fitted mcglm.
#'
#' @param object a fitted mcglm object.
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of
#' number or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#' @param ... additional arguments affecting the confidence interval produced.
#' Note that there is no extra options for
#' mcglm object class.
#' @return A data.frame with confidence intervals, parameters names,
#' response number and parameters type.
#' @export

confint.mcglm <- function(object, parm, level = 0.95, ...) {
    temp <- coef(object, std.error = TRUE)
    if(missing(parm)){parm <- 1:length(temp$Estimates)}
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    ci <- temp$Estimates + temp$Std.error %o% fac
    colnames(ci) <- paste(format(a, 2), "%", sep = "")
    rownames(ci) <- temp$Parameters
    return(ci[parm])
}
