#' @title MANOVA-type test for mcglm objects.
#' @name mc_manova_disp
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION BE CAREFUL!
#' MANOVA-type test for multivariate covariance generalized
#' linear models. This function is specify for the dispersion components.
#'
#' @param object an object of \code{mcglm} class.
#' @param idx vector of indexes for parameter tests.
#' @param names vector of names to appear in the ANOVA table.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#' @keywords internal
#' @return MANOVA table for mcglm objects.
#'
#' @export

# manova for objects of mcglm class ----------------------------------
mc_manova_disp <- function(object, idx, names, ...) {
  disp <- coef(object, type = "tau")[,1]
  n_disp <- length(disp)
  Parameters <- coef(object, type =  "tau")$Parameters
  VCOV <- vcov(object)[Parameters, Parameters]
  ## Conditional variance-covariance model
  FF <- mc_build_F(vector = idx)
  G <- Diagonal(length(object$mu_list), 1)
  CC <- lapply(FF, function(x, G){kronecker(G,x)}, G = G)
  N <- object$n_obs
  test_W <- c()
  df <- c()
  p_value <- c()
  for(i in 1:length(CC)) {
    test_W[i] <- as.numeric(t(CC[[i]]%*%disp)%*%
                              solve(CC[[i]]%*%VCOV%*%t(CC[[i]]))
                            %*%(CC[[i]]%*%disp))
    df[i] <- dim(CC[[i]])[1]
    p_value[i] <- stats::pchisq(test_W[i], df = df[i], lower.tail = FALSE)
  }
  out <- data.frame("Effects" = names, "Df" = df,
                    "Hotelling-Lawley" = round(test_W/N,3),
                    "Qui-square" = round(test_W,3),
                    "p-value" = round(p_value, 3))
  return(out)
}
