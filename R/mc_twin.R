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
#' @param formula internal.
#' @param structure model type options are full, flex, uns, ACE, ADE,
#' AE, CE and E. See example for details.
#' @param data data set.
#'
#' @source Bonat, W. H. (2018). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, 84(4):1--30.
#'
#' @return A list of matrices of \code{dgCMatrix} class.
#'
#' @seealso \code{mc_id}, \code{mc_dist}, \code{mc_car},
#' \code{mc_rw}, \code{mc_ns}, \code{mc_dglm} and \code{mc_mixed}.
#'
#' @examples
#' id <- rep(1:5, each = 4)
#' id.twin <- rep(1:2, 10)
#' @importFrom stats relevel
#' @export

mc_twin <- function(id, twin.id, type, replicate = NULL, structure, data) {
  # Checking the structure
  n_levels <- length(which(levels(data[[type]]) %in% c("mz","dz")))
  if(n_levels != 2) {
    stop("Levels of type column should be mz and dz.")
  }
  classe_twin_id <- class(data[[twin.id]])
  if(classe_twin_id != "factor") {
    data[[twin.id]] <- as.factor(data[[twin.id]])
    warning("Converted twin.id class to factor.")
  }
  if(!is.null(replicate)) {
    classe_replicate <- class(data[[twin.id]])
    if(classe_replicate != "factor") {
      data[[replicate]] <- as.factor(data[[replicate]])
      warning("Converted replicate class to factor.")
    }
  }
  # mz is set up as reference level
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
    data[[type]] <- relevel(data[[type]], ref = "mz")
    warning("Type reference is mz.")
    if(is.null(replicate)) {
      formula <- as.formula(paste("~", paste(twin.id, type, sep = "*"), sep = ""))
    }
    if(!is.null(replicate)) {
      formula <- as.formula(paste("~", paste(paste(twin.id, type, sep = "*"),
                                             replicate, sep="*"), sep = ""))
    }
    output <- mc_twin_full(id = id, twin.id = twin.id, type = type,
                 replicate = replicate, formula = formula, data = data)
  }
  if(structure == "flex") {
    data[[type]] <- relevel(data[[type]], ref = "mz")
    warning("Type reference is mz.")
    if(is.null(replicate)) {
      formula <- as.formula(paste("~",type))
    }
    if(!is.null(replicate)) {
      formula <- as.formula(paste("~", paste(paste(type, sep = ""),
                                             replicate, sep="*"), sep = ""))
    }
    formula <- as.formula(paste("~", type))
    output <- mc_twin_full(id = id, twin.id = twin.id, type = type,
                           replicate = replicate, formula = formula,
                           data = data)
  }
  if(structure == "uns") {
    data[[type]] <- relevel(data[[type]], ref = "mz")
    warning("Type reference is mz.")
    output <- mc_twin_full(id = id, twin.id = twin.id, type = type,
                           replicate = replicate, formula = ~ 1, data = data)
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
  VV <- mc_dglm(formula, id = id, data = data)
  MAT_MZ <- mc_ns(id = id, data = data)
  MAT_DZ <- mc_ns(id = id, data = data, group = type, marca = "mz")
  output <- c(VV, MAT_MZ, MAT_DZ)
  return(output)
}




