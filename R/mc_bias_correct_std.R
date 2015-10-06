#' Bias-corrected standard error for regression parameters
#'
#' @description Compute bias-corrected standard error for regression parameters in the context
#' of clustered observations. It is also robust and has improved finite sample properties.
#'
#' @param object An object of mcglm class.
#' @param id a vector which identifies the clusters. The length and order of id should be the
#' same as the number of observations. Data are assumed to be sorted so that observations on a cluster
#' are contiguous rows for all entities in the formula.
#' @return A matrix. Note that the function assumes that the data are in the correct order.
#' @export

mc_bias_corrected_std <- function(object, id) {
  inv_M <- -object$inv_S_beta
  temp_data <- data.frame(res = object$residuals, id)
  temp_data_group <- split(temp_data, temp_data$id)
  D <- bdiag(lapply(object$mu_list, function(x) x$D))
  R <- bdiag(lapply(temp_data_group, function(x) {
        tcrossprod(x[, 1])
    }))
  uni_id <- unique(id)
  n_id <- length(uni_id)
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
    I <- Diagonal(dim(data)[1], 1)
    inv_IH <- solve(I - H)
    Vbeta = inv_M%*%(t(D)%*%object$inv_C%*%inv_IH%*%R%*%inv_IH%*%
                       object$inv_C%*%D)%*%inv_M
    output = sqrt(diag(Vbeta))
    return(output)
}

