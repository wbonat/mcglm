#' @title Confidence Intervals for mcglm
#' @name confint.mcglm
#'
#' @description Computes confidence intervals for parameters in a fitted
#'     \code{mcglm} model.
#'
#' @param object a fitted \code{mcglm} object.
#' @param parm a specification of which parameters are to be given
#'     confidence intervals, either a vector of number or a vector of
#'     strings. If missing, all parameters are considered.
#' @param level the nominal confidence level.
#' @param ... additional arguments affecting the confidence interval
#'     produced. Note that there is no extra options for \code{mcglm}
#'     object class.
#'
#' @return A \code{data.frame} with confidence intervals, parameters
#'     names, response number and parameters type.
#'
#' @method confint mcglm
#'
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#' @export

confint.mcglm <- function(object, parm, level = 0.95, ...) {
    temp <- coef(object, std.error = TRUE)
    if (missing(parm)) {
        parm <- 1:length(temp$Estimates)
    }
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    ci <- temp$Estimates + temp$Std.error %o% fac
    colnames(ci) <- paste0(format(a, 2), "%")
    rownames(ci) <- temp$Parameters
    return(ci[parm])
}
