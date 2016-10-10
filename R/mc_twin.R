#' @title  Twin Models Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_twin} helps to build the components
#' of the matrix linear predictor associated with ACDE models for
#' analysis of twin data.
#'
#' @param id name of the column (string) containing the twin index.
#' It should be the same index (number) for both twins.
#' @param twin.id name of the column (string) containing the twin index
#' inside the pair. In general 1 for the first twin and 2 for the second
#' twin.
#' @param type name of the column (string) containing the indication of
#' the twin as mz or dz. It should be a factor with only two levels mz and dz.
#' Be sure that the reference level is mz.
#' @param replicate name of the column (string) containing the index
#' for more than one observation taken at the same twin pair. It is used
#' for example in twin longitudinal studies. In that case, the replication
#' column should contain the time index.
#' @param structure model type options are full, flex, uns, ACE, ADE,
#' AE, CE and E. See example for details.
#' @param data data set.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @return A list of matrices of \code{dgCMatrix} class.
#'
#' @seealso \code{mc_id}, \code{mc_dist}, \code{mc_car},
#' \code{mc_rw}, \code{mc_ns}, \code{mc_dglm} and \code{mc_mixed}.
#'
#' @examples
#' id <- rep(1:5, each = 4)
#' id.twin <- rep(1:2, 10)
#' @export

mc_twin <- function(id, twin.id, type, replicate = NULL, structure, data) {
  # Checking the structure
  n_levels <- length(which(levels(data[[type]]) %in% c("mz","dz")))
  if(n_levels != 2) {
    stop("Levels of type column should be mz and dz.")
  }
  # mz is set up as reference level
  data[[type]] <- relevel(data[[type]], ref = "mz")
  # Models structures
  if(structure == "ACE") {
    E <- mc_id(data)
    A <- mc_twin_bio(id = id, twin.id = twin.id, replicate = replicate,
                     type = type, structure = "A", data = data)
    C <- mc_twin_bio(id = id, twin.id = twin.id, replicate = replicate,
                     type = type, structure = "C", data = data)
    output <- c(E,A,C)
  }
  if(structure == "ADE") {
    E <- mc_id(data)
    A <- mc_twin_bio(id = id, twin.id = twin.id, replicate = replicate,
                     type = type, structure = "A", data = data)
    D <- mc_twin_bio(id = id, twin.id = twin.id, replicate = replicate,
                     type = type, structure = "D", data = data)
    output <- c(E,A,D)
  }
  if(structure == "AE") {
    E <- mc_id(data)
    A <- mc_twin_bio(id = id, twin.id = twin.id, replicate = replicate,
                     type = type, structure = "A", data = data)
    output <- c(E,A)
  }
  if(structure == "CE") {
    E <- mc_id(data)
    C <- mc_twin_bio(id = id, twin.id = twin.id, replicate = replicate,
                     type = type, structure = "C", data = data)
    output <- c(E,C)
  }
  if(structure == "E") {
    E <- mc_id(data)
    output <- E
  }
  if(structure == "full") {
    if(is.null(replicate)) {
      formula <- as.formula(paste("~",twin.id, sep = ""))
    }
    if(!is.null(replicate)) {
      formula <- as.formula(paste("~",paste(twin.id, replicate, sep="*"), sep = ""))
    }
    output <- mc_twin_full(id = id, twin.id = twin.id, type = type,
                 replicate = replicate, formula = formula, data = data)
  }
  if(structure == "flex") {
    output <- mc_twin_full(id = id, twin.id = twin.id, type = type,
                           replicate = replicate, formula = ~ 1, data = data)
  }
  if(structure == "uns") {
    output <- mc_twin_full(id = id, twin.id = twin.id, type = type,
                           replicate = replicate, formula = ~ 1, data = data)
    output <- output[c(1,3:length(output))]
  }
  return(output)
}

#' @rdname mc_twin
mc_twin_bio <- function(id, twin.id, type, replicate = NULL,
                        structure, data) {
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
    stop("Model requires equal number of observations per twin pair. \n")
  }
  M_list <- list()
  for(i in 1:length(data.twin)) {
    twin_temp <- data.twin[[i]]
    twin_temp$COD_ORDER1 <- 1:dim(twin_temp)[1]
    twin_temp <- twin_temp[order(twin_temp[twin.id]),]
    twin_temp$COD_ORDER2 <- 1:dim(twin_temp)[1]
    if(!is.null(replicate)) {
      n_replicate <- length(unique(twin_temp[[replicate]]))
      R_matrix <- Diagonal(n_replicate, 1)
    }
    if(is.null(replicate)) {
      n_replicate <- 1
      R_matrix <- Diagonal(n_replicate, 1)
    }
    if(unique(twin_temp[type]) == "mz") {
      twin_temp = twin_temp[order(twin_temp$COD_ORDER1),]
      M_temp <- kronecker(MZ, R_matrix)[twin_temp$COD_ORDER2,twin_temp$COD_ORDER2]
    }
    if(unique(twin_temp[type]) == "dz") {
      twin_temp = twin_temp[order(twin_temp$COD_ORDER1),]
      M_temp <- kronecker(DZ, R_matrix)[twin_temp$COD_ORDER2,twin_temp$COD_ORDER2]
    }
    M_list[[i]] <- M_temp
  }
  output <- forceSymmetric(bdiag(M_list))
  return(list(output))
}


#' @rdname mc_twin
mc_twin_full <- function(id, twin.id, type, replicate, formula, data) {
  if(is.null(replicate)) {data[replicate] <- 1}
  data_id <- split(data, data[id])
  VV_MZ <- list()
  COV_MZ <- list()
  VV_DZ <- list()
  COV_DZ <- list()
  for(i in 1:length(data_id)) {
    if(unique(data_id[[i]][type][1]) == "mz") {
      VV_MZ[[i]] <- mc_dglm(formula,
                            data = data_id[[i]], id = id)
      COV_MZ[[i]] <- mc_ns(id = id, data = data_id[[i]])
      VV_DZ[[i]] <- VV_MZ[[i]]
      COV_DZ[[i]] <- COV_MZ[[i]]
    }
    if(unique(data_id[[i]][type]) ==  "dz") {
      VV_DZ[[i]] <- mc_dglm(formula,
                            data = data_id[[i]], id = id)
      COV_DZ[[i]] <- mc_ns(id = id, data = data_id[[i]])
      VV_MZ[[i]] <- lapply(VV_DZ[[i]], function(x){0*x})
      COV_MZ[[i]] <- lapply(COV_DZ[[i]], function(x){0*x})
    }
  }
  MAT_BASE_VAR <- list()
  MAT_BASE_COV <- list()
  MAT_DZ_VAR <- list()
  MAT_DZ_COV <- list()
  for(i in 1:length(VV_MZ[[1]])) {
    MAT_BASE_VAR[[i]] <- bdiag(VV_MZ[[1]][[i]],VV_DZ[[1]][[i]])
    MAT_DZ_VAR[[i]] <- bdiag(VV_MZ[[2]][[i]],VV_DZ[[2]][[i]])
  }

  for(i in 1:length(COV_MZ[[1]])) {
    MAT_BASE_COV[[i]] <- bdiag(COV_MZ[[1]][[i]],COV_DZ[[1]][[i]])
    MAT_DZ_COV[[i]] <- bdiag(COV_MZ[[2]][[i]],COV_DZ[[2]][[i]])
  }
  saida <- c(MAT_BASE_VAR, MAT_DZ_VAR, MAT_BASE_COV, MAT_DZ_COV)
  return(saida)
}


