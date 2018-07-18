#' @title Complete Data Set with NA
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_complete_data} completes a data
#' set with NA values for helping to construct the components of the
#' matrix linear predictor in models that require equal number of
#' observations by subjects (id).
#'
#' @param data a data.frame to be completed with NA.
#' @param id name of the column (string) containing the subject id.
#' @param index name of the column (string) containing the index to be completed.
#' @param id.exp how the index is expected to be for all subjects.
#' @return A data.frame with the same number of observations by subject.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @seealso \code{mc_dglm}, \code{mc_ns}, \code{mc_ma} and \code{mc_rw}.
#' @export


mc_complete_data <- function(data, id, index, id.exp) {
  data.id <- split(data, data[id])
  ncol <- dim(data)[2]
  NN <- rep(NA, ncol)
  temp_list <- list()
  n.id <- length(data.id)
  for(j in 1:n.id) {
    temp <- data.frame(matrix(NA,length(id.exp),ncol))
    for(i in 1:length(id.exp)) {
      data_temp <- data.id[[j]][data.id[[j]][index] == id.exp[i],]
      if(nrow(data_temp) == 0) {temp[i,] <- NN}
      if(nrow(data_temp) != 0) {temp[i,] <- c(data_temp)}
    }
    temp_list[[j]] <- temp
  }
  data_complete <- do.call(rbind, temp_list)
  names(data_complete) <- names(data)
  data_complete[index] <- rep(id.exp, n.id)
  data_complete[id] <- rep(1:n.id, each = length(id.exp))
  return(data_complete)
}
