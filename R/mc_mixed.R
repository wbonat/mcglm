#' Mixed model structure
#'
#' @description Builds a mixed model structure
#'
#' @param formula A formula model to build the matrix linear predictor (see Details).
#' @param data The data set to be used.
#' @return A list matrices.
#' @export

mc_mixed <- function(formula, data) {
    X <- model.matrix(as.formula(formula), data = data)
    ass <- attr(X, "assign")
    K <- split(1:length(ass), ass)
    u <- 1:length(K)
    int <- 2
    m <- mapply(FUN = rep, u, MoreArgs = list(times = 2), SIMPLIFY = FALSE)
    if( length(K) > 1) {
    n <- combn(u, min(c(length(u), int)), simplify = FALSE)
    index <- append(m, n)
    }
    if(length(K) == 1) {
        index = m
    }
    output <- list()
    for(i in 1:c(length(index))) {
        if(index[[i]][1] == index[[i]][2]) {
            output[[i]] <- forceSymmetric(Matrix(tcrossprod(Xi <- X[, K[[index[[i]][1]]]]), sparse = TRUE))
        }
        if(index[[i]][1] != index[[i]][2]) {
            Xi <- X[, K[[index[[i]][1]]]]
            Xj <- X[, K[[index[[i]][2]]]]
            output[[i]] <- forceSymmetric(Matrix(tcrossprod(Xi, Xj) + tcrossprod(Xj, Xi), sparse = TRUE))
        }
    }
    return(output)
}
