#' @title  Random Walk Models Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_rw} builds the components of the
#'    matrix linear predictor associated with random walk models.
#'    This function is generaly used for the analysis of longitudinal
#'    and times series data. The user can specify the order of the random
#'    walk process.
#'
#' @param id name of the column (string) containing the subject index.
#'           Note that this structure was designed to deal with
#'           longitudinal data. For times series data use the same
#'           \code{id} for all observations (one unit sample).
#' @param time name of the column (string) containing the index
#'        indicating the time.
#' @param data data set.
#' @param order order of the random walk model.
#' @param proper logical.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @return If \code{proper = FALSE} a matrix of \code{dgCMatrix} class.
#'         If \code{proper = TRUE} a list with two matrices of
#'         \code{dgCMatrix} class.
#' @seealso \code{mc_id}, \code{mc_dist}, \code{mc_car},
#' \code{mc_ma}, \code{mc_mixed} and \code{mc_compute_rho}.
#' @examples
#' id <- rep(1:2, each = 4)
#' time <- rep(1:4, 2)
#' data <- data.frame("id" = id, "time" = time)
#' mc_rw(id = "id", time = "time", data = data, order = 1, proper = FALSE)
#' mc_rw(id = "id", time = "time", data = data, order = 1, proper = TRUE)
#' mc_rw(id = "id", time = "time", data = data, order = 2, proper = TRUE)
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
