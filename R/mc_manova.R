#' @title MANOVA-type test for mcglm objects.
#' @name mc_manova
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION BE CAREFUL!
#' MANOVA-type test for multivariate covariance generalized
#' linear models.
#'
#' @param object an object of \code{mcglm} class.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return MANOVA table for mcglm objects.
#'
#' @export

# manova for objects of mcglm class ----------------------------------
mc_manova <- function (object, ...)
{
  beta <- coef(object, type = "beta")[, 1]
  n_beta <- length(beta)
  VCOV <- vcov(object)[1:n_beta, 1:n_beta]
  FF <- mc_build_F(vector = attr(object$list_X[[1]], "assign"))
  G <- Diagonal(length(object$mu_list), 1)
  CC <- lapply(FF, function(x, G) {
    kronecker(G, x)
  }, G = G)
  N <- object$n_obs - dim(object$list_X[[1]])[2]
  test_W <- c()
  df <- c()
  p_value <- c()
  for (i in 1:length(CC)) {
    test_W[i] <- as.numeric(t(CC[[i]] %*% beta) %*% solve(CC[[i]] %*%
                                                            VCOV %*% t(CC[[i]])) %*% (CC[[i]] %*% beta))
    df[i] <- dim(CC[[i]])[1]
    p_value[i] <- stats::pchisq(test_W[i], df = df[i], lower.tail = FALSE)
  }
  names <- c("Intercept", attr(stats::terms(object$linear_pred[[1]]),
                               "term.labels"))
  out <- data.frame(Effects = names, Df = df,
                    `Hotelling-Lawley` = round(test_W/N,4),
                    `Qui-square` = round(test_W, 4),
                    `p-value` = round(p_value, 4))
  return(out)
}
