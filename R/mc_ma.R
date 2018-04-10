#' @title  Moving Average Models Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_ma} helps to build the components
#' of the matrix linear predictor associated with moving average models.
#' This function is generaly used for the analysis of longitudinal and
#' times series data. The user can specify the order of the moving
#' average process.
#'
#' @param id name of the column (string) containing the subject index.
#' Note that this structure was designed to deal with longitudinal data.
#' For times series data use the same \code{id} for all observations
#' (one unit sample).
#' @param time name of the column (string) containing the index indicating
#' the time.
#' @param data data set.
#' @param order order of the moving average process.
#'
#' @details This function was designed mainly to deal with longitudinal
#' data, but can also be used for times series analysis. In that case,
#' the \code{id} argument should contain only one index. It pretends a
#' longitudinal data taken just for one individual or unit sample. This
#' function is a simple call of the \code{\link[Matrix]{bandSparse}}
#' function from the \code{Matrix} package.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @return A matrix of \code{dgCMatrix} class.
#'
#' @seealso \code{mc_id}, \code{mc_dist}, \code{mc_car},
#' \code{mc_rw} and \code{mc_mixed}.
#'
#' @examples
#' id <- rep(1:2, each = 4)
#' time <- rep(1:4, 2)
#' data <- data.frame("id" = id, "time" = time)
#' mc_ma(id = "id", time = "time", data = data, order = 1)
#' mc_ma(id = "id", time = "time", data = data, order = 2)
#'
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
  data.id <- split(data, data[id], drop = TRUE)
  DD <- sum(abs(diff(do.call(c,lapply(data.id, function(x)dim(x)[1])))))
  if( DD != 0) {
    stop("Model requires equal number of observations by id. \n")
  }
  for(i in 1:length(data.id)) {
    NN <- dim(data.id[[i]])[1]
    ordem <- as.numeric(data.id[[i]][[time]])
    Z1.list[[i]] <- mc_ma_aux(n = NN, order = order)[ordem,ordem]
  }
  ordem2 <- order(data$id2)
  output <- list("Z1" = bdiag(Z1.list)[ordem2,ordem2])
  return(output)
}
