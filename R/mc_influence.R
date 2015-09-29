#' Influence measures
#'
#' @description Compute influence measures for multivariate covariance generalized linear models.
#' Leverage, DFBETA and Cook's distance for unit sample and observations.
#'
#' @param object An object of mcglm class.
#' @param id a vector which identifies the clusters. The length and order of id should be the
#' same as the number of observations. Data are assumed to be sorted so that observations on a cluster
#' are contiguous rows for all entities in the formula.
#' @return A matrix. Note that the function assumes that the data are in the correct order.
#' @export

mc_influence <- function(object, id) {
    inv_M <- -object$inv_S_beta
    M <- solve(inv_M)
    temp_data <- data.frame(res = object$residuals, id)
    temp_data_group <- split(temp_data, temp_data$id)
    D <- bdiag(lapply(object$mu_list, function(x) x$D))
    tD_invC <- t(D) %*% object$inv_C
    H <- D %*% inv_M %*% tD_invC
    leverage_obs <- diag(H)
    leverage_group <- tapply(leverage_obs, id, sum)
    I <- Diagonal(object$n_obs, 1)
    n_group <- length(temp_data_group)
    indexes <- matrix(NA, ncol = 2, nrow = n_group)
    n_obs_group <- table(id)
    indexes[1, ] <- c(1, as.numeric(n_obs_group[1]))
    DFBETA_clust <- list()
    D_temp <- D[c(indexes[1, 1]:indexes[1, 2]), ]
    inv_C_temp <- object$inv_C[c(indexes[1, 1]:indexes[1, 2]), c(indexes[1, 1]:indexes[1, 2])]
    C_temp <- object$C[c(indexes[1, 1]:indexes[1, 2]), c(indexes[1, 1]:indexes[1, 2])]
    H_temp <- H[c(indexes[1, 1]:indexes[1, 2]), c(indexes[1, 1]:indexes[1, 2])]
    res_temp <- object$residuals[indexes[1, 1]:indexes[1, 2]]
    DFBETAOij <- list()
    padroniza <- function(x, M) {
        as.numeric((t(as.numeric(x)) %*% M %*% as.numeric(x))/dim(M)[1])
    }
    dfbetaOij <- function(D_temp, C_temp, inv_C_temp, res_temp) {
        DFBETA_temp <- matrix(NA, ncol = dim(D_temp)[2], nrow = dim(D_temp)[1])
        k = 1
        for (j in 1:length(res_temp)) {
            Dij <- D_temp[j, k] - C_temp[k, -j] %*% inv_C_temp[-k, -j] %*% D_temp[-j, ]
            rij <- res_temp[j] - C_temp[k, -j] %*% inv_C_temp[-k, -j] %*% res_temp[-j]
            Vij <- C_temp[k, j] - C_temp[k, -j] %*% inv_C_temp[-k, -j] %*% C_temp[-j, k]
            Hij <- try(Dij %*% object$inv_S_beta %*% t(Dij) %*% solve(Vij), silent = TRUE)
            if (class(Hij) == "try-error") {
                DFBETA_temp[j, ] <- NA
            }
            if (class(Hij) != "try-error") {
                DFBETA_temp[j, ] <- as.numeric(t(object$inv_S_beta %*% t(Dij) %*% (rij/(Vij - (1 - Hij)))))
            }
        }
        return(DFBETA_temp)
    }
    DFBETA_clust[[1]] <- inv_M %*% t(D_temp) %*% inv_C_temp %*% res_temp
    DFBETAOij[[1]] <- dfbetaOij(D_temp, C_temp, inv_C_temp, res_temp)
    for (i in 2:n_group) {
        indexes[i, ] <- c(indexes[i - 1, ][2] + 1, n_obs_group[i] + indexes[i - 1, ][2])
        D_temp <- D[c(indexes[i, 1]:indexes[i, 2]), ]
        inv_C_temp <- object$inv_C[c(indexes[i, 1]:indexes[i, 2]), c(indexes[i, 1]:indexes[i, 2])]
        C_temp <- object$C[c(indexes[i, 1]:indexes[i, 2]), c(indexes[i, 1]:indexes[i, 2])]
        H_temp <- H[c(indexes[i, 1]:indexes[i, 2]), c(indexes[i, 1]:indexes[i, 2])]
        res_temp <- object$residuals[indexes[i, 1]:indexes[i, 2]]
        D_temp <- matrix(D_temp, nrow = n_obs_group[i])
        DFBETA_clust[[i]] <- inv_M %*% t(D_temp) %*% inv_C_temp %*% res_temp
        if (n_obs_group[i] == 1) {
            DFBETAOij[[i]] <- t(as.matrix(DFBETA_clust[[i]]))
        }
        if (n_obs_group[i] != 1) {
            DFBETAOij[[i]] <- dfbetaOij(D_temp, C_temp, inv_C_temp, res_temp)
        }
    }
    DFBETA <- lapply(DFBETA_clust, as.matrix)
    DFBETA <- lapply(DFBETA, t)
    DFBETA <- ldply(DFBETA, data.frame)
    names(DFBETA) <- object$beta_names[[1]]
    DFBETAOij <- ldply(DFBETAOij, data.frame)
    names(DFBETAOij) <- object$beta_names[[1]]
    DCLSi <- apply(as.matrix(DFBETA), MARGIN = 1, FUN = padroniza, M = M)
    DCLOij <- apply(as.matrix(DFBETAOij), MARGIN = 1, FUN = padroniza, M = M)
    std.error <- coef(object, std.error = TRUE, type = "beta")$Std.error
    DFBETA_temp <- t(apply(DFBETA, MARGIN = 1, FUN = function(x, std.error) {
        as.numeric(x/std.error)
    }, std.error = std.error))
    DFBETAOij_temp <- t(apply(DFBETAOij, MARGIN = 1, FUN = function(x, std.error) {
        as.numeric(x/std.error)
    }, std.error = std.error))
    output_clust <- data.frame(Leverage = leverage_group, DFBETA = DFBETA_temp, Cook = DCLSi)
    output_obs <- data.frame(Leverage = leverage_obs, DFBETA = DFBETAOij_temp, Cook = DCLOij)
    output <- list(Id = output_clust, Observations = output_obs)
    return(output)
}
