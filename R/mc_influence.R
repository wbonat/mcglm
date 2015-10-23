#' @title Influence measures for McGLMs
#'
#' @description Compute influence measures for multivariate covariance
#' generalized linear models. Leverage, DFBETA and Cook's distance
#' for unit sample and observations.
#' @param object An object of \code{mcglm} class.
#' @param id a vector which identifies the clusters.
#' The length and order of id should match with the number of
#' observations.
#' @return A list with influence measures for cluster and observations.
#'
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#' @export

mc_influence <- function(object, id) {
  temp_data <- data.frame(object$residuals, id)
  names(temp_data) <- c("res", "id")
  data_id <- split(temp_data, temp_data$id)
  n_id <- length(data_id)
  D <- bdiag(lapply(object$mu_list, function(x) x$D))
  n_beta <- dim(D)[2]
  R <- bdiag(lapply(data_id, function(x) {
    tcrossprod(x[, 1])
  }))
  # Leverage for observations
  inv_M <- -object$inv_S_beta
  M <- solve(inv_M)
  uni_id <- as.character(unique(id))
  Hi <- list()
  Di <- list()
  inv_Ci <- list()
  for (i in 1:n_id) {
    idTF <- id == uni_id[i]
    if(sum(idTF) == 1) {
      Di[[i]] <- Matrix(D[idTF,], nrow = sum(idTF), ncol = dim(D)[2])
    } else {
      Di[[i]] <- D[idTF,]
    }
    inv_Ci[[i]] <- object$inv_C[idTF,idTF]
    Hi[[i]] <- Di[[i]]%*%inv_M%*%t(Di[[i]])%*%inv_Ci[[i]]
  }
  H <- bdiag(Hi)
  # Leverage for observations
  Hobs <- diag(H)
  # Leverage for clusters
  Hid <- tapply(Hobs, id, mean)
  # DFBETAs for clusters
  I <- Diagonal(dim(temp_data)[1], 1)
  inv_IH <- solve(I - H)
  DFBETACi <- list()
  DFBETAOij <- list()
  DCLSi <- c()
  for(i in 1:n_id) {
    idTF <- id == uni_id[i]
    if(sum(idTF) == 1) {
      D_i <- Matrix(D[idTF,], nrow = sum(idTF), ncol = n_beta)
      inv_C_i <- Matrix(object$inv_C[idTF,idTF],ncol = 1, nrow = 1)
    } else {
      D_i <- D[idTF,]
      inv_C_i <- object$inv_C[idTF,idTF]
    }
    inv_IH_i = inv_IH[idTF,idTF]
    r_i <- object$residuals[idTF]
    DFBETACi[[i]] <- t(inv_M%*%t(D_i)%*%inv_C_i%*%inv_IH_i%*%r_i)
    DCLSi[i] <- as.numeric(DFBETACi[[i]]%*%M%*%t(DFBETACi[[i]]))/n_beta
    if(dim(inv_C_i)[1] == 1) {DFBETAOij[[i]] <- DFBETACi[[i]]
    } else {
    DFBETAOij[[i]] <- mc_dfbetaOij(Di = D_i, Ci = object$C[idTF,idTF],
                                   inv_Ci = inv_C_i, ri = r_i,
                                   inv_M = inv_M)
    }
  }
  DFBETACi <- do.call(rbind, DFBETACi)
  DFBETAOij <- do.call(rbind, DFBETAOij)
  DCOij <- apply(DFBETAOij, MARGIN = 1,
                function(x,M,n_beta)as.numeric((t(x)%*%M%*%x)/n_beta),
                M = M, n_beta = n_beta)
  std.error <- coef(object, type = "beta", std.error = TRUE)$Std.error
  DFBETACi = data.frame(as.matrix(DFBETACi))/std.error
  DFBETAOij = data.frame(as.matrix(DFBETAOij))/std.error
  names(DFBETACi) <- do.call(c, object$beta_names)
  names(DFBETAOij) <- do.call(c, object$beta_names)
  DFBETACi$Leverage <- as.numeric(Hid)
  DFBETACi$Cook <- as.numeric(DCLSi)
  DFBETAOij$Leverage <- as.numeric(Hobs)
  DFBETAOij$Cook <- as.numeric(DCOij)
  DFBETAOij$Id <- rownames(object$data)
  DFBETACi$Id <- uni_id
  output <- list("Id" = DFBETACi, "Obs" = DFBETAOij)
  return(output)
}

#' @title Influence measures for McGLMs
#'
#' @description Compute influence measures for multivariate covariance
#' generalized linear models. Leverage, DFBETA and Cook's distance
#' for observations. Auxiliar function for \code{mc_influence}.
#' @param Di D matrix for the cluster i.
#' @param Ci C matrix for the cluster i.
#' @param inv_Ci Inverse of C matrix for the cluster i.
#' @param ri Residual vector for the cluster i.
#' @param inv_M Inverse of variance/covariance of regression parameters.
#' @return Matrix with the DFBETA for observation in the cluster i.
#'
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#' @export

mc_dfbetaOij <- function(Di, Ci, inv_Ci, ri, inv_M) {
  nobs <- dim(inv_Ci)[1]
  nbeta <- dim(Di)[2]
  DFBETAOij <- matrix(NA, ncol = nbeta, nrow = nobs)
  for (i in 1:nobs) {
    Ci.jj <- Matrix(Ci[-i,i])
    Cij.j <- t(Ci.jj)
    inv_Ci.j <- inv_Ci[-i,-i]
    Di.j <- Di[-i,]
    Dij_temp <- Di[i,] - Cij.j%*%inv_Ci.j%*%Di.j
    rij_temp <- as.numeric(ri[i] - Cij.j%*%inv_Ci.j%*%ri[-i])
    Cij_temp <- as.numeric(Ci[i,i] - Cij.j%*%inv_Ci.j%*%t(Cij.j))
    Hij_temp <- as.numeric(Dij_temp%*%inv_M%*%t(Dij_temp)%*%solve(Cij_temp))
    part2 <- Matrix(rij_temp/(Cij_temp*(1-Hij_temp)))
    DFBETAOij[i,] <- as.numeric(t(inv_M%*%t(Dij_temp)%*%part2))
  }
  return(DFBETAOij)
}



