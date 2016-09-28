#' @title Double Generalized Linear Models Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_dglm} builds the components
#' of the matrix linear predictor used for fitting double generalized
#' linear models.
#'
#' @param formula a formula spefying the components of the covariance
#' structure.
#' @param id name of the column (string) containing the subject index.
#' (If ts is not repeated measures, use id = 1 for all observations).
#' @param data data set.
#' @return A list of a diagonal matrices, whose values are given by
#' the covariates assumed to describe the covariance structure.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @seealso \code{mc_id}, \code{mc_dist}, \code{mc_ma}, \code{mc_rw}
#' \cr and \code{mc_mixed}.
#' @export

mc_dglm <- function(formula, id, data) {
    mc_dglm_aux <- function(formula, data) {
    Z <- model.matrix(formula, data = data)
    N <- dim(Z)[1]
    ZZ <- list()
    for(i in 1:dim(Z)[2]) {
        ZZ[[i]] <- Diagonal(N, Z[,i])
    }
    return(ZZ)
    }
    data.id <- split(data, data[id])
    mat.list <- list()
    for(i in 1:length(data.id)) {
        mat.list[[i]] <- mc_dglm_aux(formula = formula, data = data.id[[i]])
    }
    dglm_list <- list()
    for(i in 1:length(mat.list[[1]])) {
        dglm_list[[i]] <- forceSymmetric(bdiag(lapply(mat.list, function(x)x[[i]])))
    }
    return(dglm_list)
}
