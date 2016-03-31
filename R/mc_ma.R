#' Moving average models
#'
#' @description Builds moving average model matrix of order k.
#'
#' @param id Subject index. Note that this structure was designed to deal with longitudinal data.
#' @param time Index indicating the time.
#' @param data Data set.
#' @param order Order of the random walk model.
#' @return A matrix of dgCMatrix class.
#' @export

mc_ma <- function(id, time, data, order = 1) {
  mc_ma_aux <- function(n, order) {
    output <- bandSparse(n, n, k = order, symmetric = TRUE)
    return(output)
  }
  data$id2 <- 1:dim(data)[1]
  data <- data[order(data[id]),]
  data$id3 <- 1:dim(data)[1]
  Z1.list <- list()
  data.id <- split(data, data[id])
  for(i in 1:length(data.id)) {
    NN <- dim(data.id[[i]])[1]
    ordem <- as.numeric(data.id[[i]][[time]])
    Z1.list[[i]] <- mc_ma_aux(n = NN, order = order)[ordem,ordem]
  }
  ordem2 <- order(data$id2)
  output <- list("Z1" = bdiag(Z1.list)[ordem2,ordem2])
  return(output)
}
