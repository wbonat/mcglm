#' @title  Twin Models Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_twin} helps to build the components
#' of the matrix linear predictor associated with ACDE models for
#' analysis of twin data.
#'
#' @param id name of the column (string) containing the twin index.
#' It should be the same index (number) for both twins.
#' @param id.twin name of the column (string) containing the twin index
#' inside the pair. In general 1 for the first twin and 2 for the second
#' twin.
#' @param type name of the column (string) containing the indication of
#' the twin as mz or dz. It should be a factor with only two levels mz and dz.
#' @param replication name of the column (string) containing the index
#' for more than one observation taken at the same twin pair. It is used
#' for example in twin longitudinal studies. In that case, the replication
#' column should contain the time index.
#' @param structure model type options are A, C and D. In general, one
#' model will use more than one component like AC or AD models. The user
#' should also use the \code{mc_id} function to create the E component
#' of the twin model. See example for details.
#' @param data data set.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @return A matrix of \code{dgCMatrix} class.
#'
#' @seealso \code{mc_id}, \code{mc_dist}, \code{mc_car},
#' \code{mc_rw}, \code{mc_ns}, \code{mc_dglm} and \code{mc_mixed}.
#'
#' @examples
#' id <- rep(1:5, each = 4)
#' id.twin <- rep(1:2, 10)
#' type <- c(rep("mz",12),rep("dz",8))
#' replication <- rep(rep(1:2, each = 2),5)
#' data <- data.frame(id, id.twin, type,replication)
#' mc_twin(twin.id = "id.twin", id = "id", type = "type",
#' structure = "A", replicate = "replication", data = data)
#' data2 <- data[order(data$id.twin),]
#' mc_twin(twin.id = "id.twin", id = "id", type = "type",
#' structure = "A", replicate = "replication", data = data2)
#' @export

mc_twin <- function(id, twin.id, type, replicate = NULL,
                    structure, data) {
  n_levels <- length(which(levels(data[type][[1]]) %in% c("mz","dz")))
  if(n_levels != 2) {
    stop("Levels of type column should be mz and dz.")
  }
  # A matrix
  if ( structure == "A" ) {
  MZ <- Matrix(c(1, 1, 1, 1), 2, 2)
  DZ <- Matrix(c(1, 0.5, 0.5, 1), 2, 2)
  }
  # C matrix
  if( structure == "C") {
  MZ <- Matrix(c(1, 1, 1, 1), 2, 2)
  DZ <- Matrix(c(1, 1, 1, 1), 2, 2)
  }
  # D matrix
  if( structure == "D") {
  MZ <- Matrix(c(1, 1, 1, 1), 2, 2)
  DZ <- Matrix(c(1, 0.25, 0.25, 1), 2, 2)
  }
  data.twin <- split(data, data[id])
  DD <- sum(abs(diff(do.call(c,lapply(data.twin, function(x)dim(x)[1])))))
  if( DD != 0) {
    stop("Model requires equal number of observations by twin. \n")
  }
  M_list <- list()
  for(i in 1:length(data.twin)) {
    twin_temp <- data.twin[[i]]
    twin_temp$COD_ORDER1 <- 1:dim(twin_temp)[1]
    twin_temp <- twin_temp[order(twin_temp[twin.id]),]
    twin_temp$COD_ORDER2 <- 1:dim(twin_temp)[1]
    if(!is.null(replicate)) {
      n_replicate <- dim(twin_temp[replicate])[1]/2
      R_matrix <- Diagonal(n_replicate, 1)
    }
    if(is.null(replicate)) {
      n_replicate <- 1
      R_matrix <- Diagonal(n_replicate, 1)
    }
    if(unique(twin_temp[type]) == "mz") {
      M_temp <- kronecker(MZ, R_matrix)[twin_temp$COD_ORDER1,twin_temp$COD_ORDER1]
    }
    if(unique(twin_temp[type]) == "dz") {
      M_temp <- kronecker(DZ, R_matrix)[twin_temp$COD_ORDER1,twin_temp$COD_ORDER1]
    }
    M_list[[i]] <- M_temp
  }
  output <- bdiag(M_list)
  return(output)
}
