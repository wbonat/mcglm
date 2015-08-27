#' Default Multivariate Covariance Generalized Linear models plotting
#'
#' @description takes a fitted mcglm object by \code{mcglm()} and plots, residuals,
#' influence diagnostic measures and algorithm check.
#'
#' @param object a fitted mcglm object as produced by \code{mcglm()}.
#' @param type Specify which graphical analysis will be performed, options are: residuals, influence
#' and algorithm.
#' @exportMethod
plot.mcglm <- function(object, type = "residuals") {
  n_resp <- length(object$beta_names)
  if(type == "residuals") {
    par(mar=c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0), mfrow = c(1,n_resp))
    for(i in 1:n_resp){
      plot(residuals(object, type = "pearson")[,i] ~ fitted(object)[,i],
           ylab = "Pearson residuals", xlab = "Fitted values")
      temp <- loess.smooth(fitted(object)[,i],residuals(object, type = "pearson")[,i])
      lines(temp$x,temp$y)
    }
  }
  if(type == "partial_residuals"){
    par(mar=c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0), mfrow = c(n_resp,c(n_cov-1)))
    for(i in 1:n_resp){
      n_cov <- dim(object$list_X[[i]])[2]
      for(j in 2:n_cov){
        plot(residuals(object, type = "pearson")[,i] ~  as.numeric(object$list_X[[i]][,j]))
        temp <- loess.smooth(as.numeric(object$list_X[[i]][,j]),residuals(object, type = "pearson")[,i])
        lines(temp$x,temp$y)
        }
    }
}
  if(type == "algorithm") {
    n_iter <- length(na.exclude(object$IterationCovariance[,1]))
    par(mar=c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0), mfrow = c(2,2))
    matplot(object$IterationRegression[1:c(n_iter+5),], type = "l", lty = 2, ylab = "Regression",
            xlab = "Iterations")
    matplot(object$IterationCovariance[1:c(n_iter+5),], type = "l", lty = 2, ylab = "Covariance",
            xlab = "Iterations")
    matplot(object$ScoreRegression[1:c(n_iter+5),], type = "l", lty = 2, ylab = "Quasi-score Regression",
            xlab = "Iterations")
    matplot(object$ScoreCovariance[1:c(n_iter+5),], type = "l", lty = 2, ylab = "Quasi-score Covariance",
            xlab = "Iterations")
  }
}
