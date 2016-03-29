#' Random walk models
#'
#' @description Builds a random walk model matrix of order k.
#'
#' @param id Subject index. Note that this structure was designed to deal with longitudinal data.
#' @param time Index indicating the time.
#' @param data Data set.
#' @param order Order of the random walk model.
#' @param proper Logical.
#' @return A matrix of dgCMatrix class.
#' @export

mc_rw <- function(id, time, data, order = 1, proper = FALSE) {
    mc_rw_aux <- function(n, order, proper = TRUE) {
        U = Matrix(diff(diag(n), diff = order),sparse = TRUE)
        Z <- forceSymmetric(t(U)%*%U)
        if(proper == TRUE) {
            Z2 <- Z
            Z1 <- Diagonal(dim(Z)[1], diag(Z))
            diag(Z2) <- 0
            output <- list("Z1" = Z1, "Z2" = Z2)
        } else {
            output <- list("Z1" = Z)
        }
        return(output)
    }
    data$id2 <- 1:dim(data)[1]
    data <- data[order(data[id]),]
    data$id3 <- 1:dim(data)[1]
    if(proper == TRUE) {
        Z1.list <- list()
        Z2.list <- list()
    } else {
        Z1.list <- list()
    }
    data.id <- split(data, data[id])
    for(i in 1:length(data.id)) {
        n <- dim(data.id[[i]])[1]
        ordem <- as.numeric(data.id[[i]][[time]])
        if(proper == TRUE) {
            ZZ <- mc_rw_aux(n = n, order = order, proper = TRUE)
            Z1.list[[i]] <- ZZ$Z1[ordem,ordem]
            Z2.list[[i]] <- ZZ$Z2[ordem,ordem]
        } else {
            ZZ <- mc_rw_aux(n = n, order = order, proper = FALSE)
            Z1.list[[i]] <- ZZ$Z1[ordem,ordem]
        }
    }
    if(proper == TRUE) {
        ordem2 <- order(data$id2)
        output <- list("Z1" = bdiag(Z1.list)[ordem2,ordem2],
                       "Z2" = bdiag(Z2.list)[ordem2,ordem2])
    } else {
        ordem2 <- order(data$id2)
        output <- list("Z1" = bdiag(Z1.list)[ordem2,ordem2])
    }
    return(output)
}
