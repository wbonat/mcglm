#' @title Non-structure Model Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_non} builds the components
#' of the matrix linear predictor used for fitting non-structured
#' covariance matrix. In general this model is hard to fit due to the
#' large number of parameters.
#'
#' @param id name of the column (string) containing the subject index.
#' Note this structure was designed to deal with longitudinal data.
#' For times series or spatial data use the same id for all observations
#' (one unit sample).
#' @param group name of the column (string) containing a group specific
#' for which the covariance should change.
#' @param marca level (string) of the column group for which the
#' covariance should change.
#' @param data data set.
#' @return A list of a n*(n-1)/2 matrices.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @seealso \code{mc_id}, \code{mc_dglm}, \code{mc_dist}, \code{mc_ma},
#' \code{mc_rw} \cr and \code{mc_mixed}.
#' @export

mc_ns <- function(id, data, group = NULL, marca = NULL) {
  mc_non_aux <- function(n.resp){
    position <- combn(n.resp,2)
    list.Derivative <- list()
    n.par <- n.resp*(n.resp-1)/2
    for(i in 1:n.par){
      Derivative <- matrix(0, ncol = n.resp, nrow = n.resp)
      Derivative[position[1,i],position[2,i]] <- Derivative[position[2,i],position[1,i]] <- 1
      list.Derivative[i][[1]] <- Derivative}
    return(list.Derivative)
  }
  data[id] <- factor(data[[id]], levels=unique(data[[id]]))
  data.id <- split(data, data[id], drop = TRUE)
  DD <- sum(abs(diff(do.call(c,lapply(data.id, function(x)dim(x)[1])))))
  if( DD != 0) {
    stop("Model requires equal number of observations by id. \n")
  }
  mat.list <- list()
  for(i in 1:length(data.id)) {
    if (!is.null(group)) {
      if (unique(data.id[[i]][group]) == marca) {
      mat.list[[i]] <- lapply(mc_non_aux(dim(data.id[[i]])[1]), function(x){0*x} )
      }
    }
    if(!is.null(group)) {
      if(unique(data.id[[i]][group]) != marca) {
        mat.list[[i]] <- mc_non_aux(dim(data.id[[i]])[1])
      }
    }
    if(is.null(group)) {
      mat.list[[i]] <- mc_non_aux(dim(data.id[[i]])[1])
    }
  }
  non_list <- list()
  for(i in 1:length(mat.list[[1]])) {
    non_list[[i]] <- bdiag(lapply(mat.list, function(x)x[[i]]))
  }
  return(non_list)
}
