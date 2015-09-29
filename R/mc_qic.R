#' Compute Quasi Information Criterion (QIC) for McGLMs.
#'
#' qic.mcglm is a function which computes the QIC for McGLMs.
#' @param object An object of mcglm class.
#' @param object.iid An object of mcglm class contained the model fitted using independent
#' covariance structure.
#' @return The QIC value.
#' @export

qic.mcglm <- function(object, object.iid) {
    mu <- fitted(object)
    obs <- object$observed
    n_resp <- dim(mu)[2]
    Q <- matrix(NA, ncol = dim(mu)[2], nrow = dim(mu)[1])
    for (i in 1:n_resp) {
        if (object$power_fixed[[i]] == FALSE) {
            power <- coef(object, type = "power", response = i)$Estimate
        }
        if (object$power_fixed[[i]] == TRUE) {
            power = object$list_initial$power[[i]]
        }
        Q[, i] <- mc_qll(y = obs[, i], mu = mu[, i], variance = object$variance[[i]], power = power)
    }
    Vbeta <- -object$inv_S_beta
    Vnull <- solve(-object.iid$inv_S_beta)
    t1 <- -2 * sum(Q)
    qic <- t1 + 2 * sum(diag(Vnull %*% Vbeta))
    return(list(Q = t1, qic = qic))
}
