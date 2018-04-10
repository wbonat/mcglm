#' @title Distance Models Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_dist} helps to build the components
#' of the matrix linear predictor using matrices based on distances.
#' This function is generaly used for the analysis of longitudinal and
#' spatial data. The idea is to use the inverse of some measure of distance
#' as for example the Euclidean distance to model the covariance structure
#' within response variables. The model can also use the inverse of
#' distance squared or high order power.
#'
#' @param id name of the column (string) containing the subject index.
#' For spatial data use the same \code{id} for all observations (one unit sample).
#' @param time name of the column (string) containing the index
#' indicating the time. For spatial data use the same index for all observations.
#' @param data data set.
#' @param method distance measure to be used.
#'
#' @details The distance measure must be one of \code{"euclidean"},
#' \code{"maximum"}, \code{"manhattan"}, \code{"canberra"},
#' \code{"binary"} or \code{"minkowski"}. This function is a customize
#' call of the \code{\link[stats]{dist}} function.
#'
#' @seealso \code{\link[stats]{dist}}, \code{mc_id},
#' \code{mc_conditional_test}, \code{mc_car}, \code{mc_ma},
#' \code{mc_rw} and \code{mc_mixed}.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @return A matrix of dgCMatrix class.
#' @examples
#' id <- rep(1:2, each = 4)
#' time <- rep(1:4, 2)
#' data <- data.frame("id" = id, "time" = time)
#' mc_dist(id = "id", time = "time", data = data)
#' mc_dist(id = "id", time = "time", data = data, method = "canberra")
#' @export

mc_dist <- function(id, time, data, method = "euclidean") {
  mc_dist_aux <- function(time, method = method, data) {
    output <- forceSymmetric(as.matrix(1/stats::dist(data[time],
                                                     method = method,
                                                     diag = TRUE,
                                                     upper = TRUE)))
    diag(output) <- 0
    return(output)
  }
  data$id2 <- 1:dim(data)[1]
  data <- data[order(data[id]),]
  data$id3 <- 1:dim(data)[1]
  Z1.list <- list()
  data.id <- split(data, data[id], drop = TRUE)
  for(i in 1:length(data.id)) {
      Z1.list[[i]] <- mc_dist_aux(time = time, method = method, data = data.id[[i]])
  }
  ordem2 <- order(data$id2)
  output <- list("Z1" = bdiag(Z1.list)[ordem2,ordem2])
  return(output)
}
