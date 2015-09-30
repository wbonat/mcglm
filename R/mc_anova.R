#' ANOVA method for McGLMs.
#'
#' @description ANOVA method for McGLMS.
#'
#' @param object an object of class mcglm, usually, a result of a call to \code{mcglm}.
#' @param ... additional arguments affecting the summary produced. Note that there is no extra options for
#' mcglm object class.
#' @export

anova.mcglm <- function(object, ...) {
    n_resp <- length(object$mu_list)
    n_beta <- lapply(object$list_X, ncol)
    idx.list <- list()
    for (i in 1:n_resp) {
        idx.list[[i]] <- rep(i, n_beta[i])
    }
    vv <- vcov(object)
    n_par <- dim(vv)[1]
    idx.vec <- do.call(c, idx.list)
    n_cov <- n_par - length(idx.vec)
    idx.vec <- c(idx.vec, rep(0, n_cov))
    temp.vcov <- list()
    temp.beta <- list()
    for (i in 1:n_resp) {
        idx.id = idx.vec == i
        temp.vcov[[i]] <- vv[idx.id, idx.id]
        temp.beta[[i]] <- coef(object, type = "beta", response = i)$Estimates
    }
    saida <- list()
    for (i in 1:n_resp) {
        idx <- attr(object$list_X[[i]], "assign")
        names <- colnames(object$list_X[[i]])
        if (names[1] == "(Intercept)") {
            idx <- idx[-1]
            names <- names[-1]
            temp.beta[[i]] <- temp.beta[[i]][-1]
            temp.vcov[[i]] <- temp.vcov[[i]][-1, -1]
        }
        n_terms <- length(unique(idx))
        X2.resp <- list()
        for (j in 1:n_terms) {
            idx.TF <- idx == j
            temp <- as.numeric(t(temp.beta[[i]][idx.TF]) %*% solve(as.matrix(temp.vcov[[i]])[idx.TF, idx.TF]) %*% temp.beta[[i]][idx.TF])
            nbeta.test <- length(temp.beta[[i]][idx.TF])
            X2.resp[[j]] <- data.frame(Covariate = names[idx.TF][1], Chi.Square = round(temp, 4), Df = nbeta.test, p.value = round(pchisq(temp,
                nbeta.test, lower.tail = FALSE), 4))
        }
        saida[[i]] <- do.call(rbind, X2.resp)
    }
    cat("Wald test for fixed effects", "\n")
    return(saida)
}
