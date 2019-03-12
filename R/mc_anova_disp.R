#' @title Anova Tables for dispersion components
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description IT IS AN EXPERIMENTAL FUNCTION BE CAREFUL!
#' Performs Wald tests of the significance for the dispersion
#' components by response variables. This function is useful
#' for joint hypothesis tests of dispersion coefficients associated with
#' categorical covariates with more than two levels. It is not designed
#' for model comparison.
#'
#' @param object an object of class \code{mcglm}, usually, a result of a
#'     call to \code{mcglm()} function.
#' @param idx_list list with indexes for parameter tests.
#' @param names_list list of names to appear in the anova table.
#' @param ... additional arguments affecting the summary produced. Note
#'     that there is no extra options for mcglm object class.
#'
#' @return A \code{data.frame} with Chi-square statistic to test the
#'     null hypothesis of a parameter, or a set of parameters, be
#'     zero. Degree of freedom (Df) and p-values.
#'     The Wald test based on the observed covariance matrix of
#'     the parameters is used.
#' @examples
#' x1 <- seq(0, 1, l = 100)
#' x2 <- gl(5, 20)
#' beta <- c(5, 0, -2, -1, 1, 2)
#' X <- model.matrix(~ x1 + x2)
#' set.seed(123)
#' y <- rnorm(100, mean = 10, sd = X%*%beta)
#' data = data.frame("y" = y, "x1" = x1, "x2" = x2, "id" = 1)
#' fit.anova <- mcglm(c(y ~ 1), list(mc_dglm(~ x1 + x2, id = "id", data)),
#'                    control_algorithm = list(tuning = 0.9), data = data)
#' X <- model.matrix(~ x1 + x2, data = data)
#' idx <- attr(X, "assign")
#' idx_list <- list("idx" = idx, "idx" = idx)
#' names_list <- list(colnames(X), colnames(X))
#' mc_anova_disp(object = fit.anova, idx = idx_list, names_list = names_list)
#'
#' @export

mc_anova_disp <- function(object, idx_list, names_list, ...) {
  n_resp <- length(object$mu_list)
  n_disp <- lapply(object$matrix_pred, length)
  idx.list <- list()
  for (i in 1:n_resp) {
    idx.list[[i]] <- rep(i, n_disp[i])
  }
  vv <- vcov(object)
  n_par <- dim(vv)[1]
  idx.vec <- do.call(c, idx.list)
  n_beta <- n_par - length(idx.vec)
  idx.vec <- c(rep(0, n_beta), idx.vec)
  temp.vcov <- list()
  temp.disp <- list()
  for (i in 1:n_resp) {
    idx.id <- idx.vec == i
    temp.vcov[[i]] <- vv[idx.id, idx.id]
    temp.disp[[i]] <- coef(object, type = "tau", response = i)$Estimates
  }
  saida <- list()
  for (i in 1:n_resp) {
    idx <- idx_list[[i]]
    names <- names_list[[i]]
    if (names[1] == "(Intercept)") {
      idx <- idx[-1]
      names <- names[-1]
      temp.disp[[i]] <- temp.disp[[i]][-1]
      temp.vcov[[i]] <- temp.vcov[[i]][-1, -1]
    }
    n_terms <- length(unique(idx))
    X2.resp <- list()
    for (j in 1:n_terms) {
      idx.TF <- idx == j
      temp <- as.numeric(
        t(temp.disp[[i]][idx.TF]) %*%
          solve(as.matrix(temp.vcov[[i]])[idx.TF, idx.TF]) %*%
          temp.disp[[i]][idx.TF])
      nbeta.test <- length(temp.disp[[i]][idx.TF])
      X2.resp[[j]] <-
        data.frame(Covariate = names[idx.TF][1],
                   Chi.Square = round(temp, 4), Df = nbeta.test,
                   p.value = round(pchisq(temp, nbeta.test,
                                          lower.tail = FALSE),
                                   4))
    }
    saida[[i]] <- do.call(rbind, X2.resp)
  }
  cat("Wald test for fixed effects\n")
  for(i in 1:n_resp) {
    cat("\n")
    print(saida[[i]])
    cat("\n")
  }
  return(invisible(saida))
}
