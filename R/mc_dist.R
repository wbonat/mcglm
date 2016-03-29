#' Distances models
#'
#' @description Builds distances model matrix.
#'
#' @param id Subject index. Note that this structure was designed to deal with longitudinal data.
#' @param time Index indicating the time.
#' @param data Data set
#' @param method The distance measure to be used. This must be one of
#' "euclidean", "maximum","manhattan","canberra","binary" or "minkowski".
#' @return A matrix of dgCMatrix class.
#' @export

mc_dist <- function(id, time, data, method = "euclidean") {
  mc_dist_aux <- function(time, method = method, data) {
    output <- forceSymmetric(as.matrix(1/dist(data[time], diag = TRUE, upper = TRUE)))
    diag(output) <- 0
    return(output)
  }
  data$id2 <- 1:dim(data)[1]
  data <- data[order(data[id]),]
  data$id3 <- 1:dim(data)[1]
  Z1.list <- list()
  data.id <- split(data, data[id])
  for(i in 1:length(data.id)) {
      Z1.list[[i]] <- mc_dist_aux(time = time, method = method, data = data.id[[i]])
  }
  ordem2 <- order(data$id2)
  output <- list("Z1" = bdiag(Z1.list)[ordem2,ordem2])
  return(output)
}
