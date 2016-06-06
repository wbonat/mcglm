#' @title  Mixed Models Structure
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description The function \code{mc_mixed} helps to build the components
#' of the matrix linear predictor associated with mixed models. It is
#' useful to model the covariance structure as a function of known
#' covariates in a linear mixed model fashion (Bonat, et. al. 2016).
#' The \code{mc_mixed} function was designed to analyse repeated measures
#' and longitudinal data, where in general the observations are taken
#' at a fixed number of groups, subjects or unit samples.
#'
#' @param formula a formula model to build the matrix linear predictor.
#'    See details.
#' @param data data set.
#' @return A list of matrices.
#'
#' @details The \code{formula} argument should be specified similar to
#' the linear predictor for the mean structure, however the first
#' component should be 0 and the second component should always
#' indicate the name of the column containing the subject
#' or unit sample index. It should be a \code{factor}. The other covariates
#' are specified after a slash "\" in the usual way. For example,
#' \code{~0 + SUBJECT/(x1 + x2)} means that the column SUBJECT contains the
#' subject or unit sample index, while the covariates that can be continuous
#' or factors are given in the columns x1 and x2. Be careful the parenthesis
#' after the "\" are mandatory, when including more than one covariate.
#' The special case where only the SUBJECT effect is requested the formula
#' takes the form \code{~ 0 + SUBJECT} without any extra covariate.
#' This structure corresponds to the well known compound symmetry structure.
#' By default the function \code{mc_mixed} include all interaction terms,
#' the users can ignore the interactions terms removing them from the
#' matrix linear predictor.
#'
#' @examples
#' SUBJECT <- gl(2, 6)
#' x1 <- rep(1:6, 2)
#' x2 <- rep(gl(2,3),2)
#' data <- data.frame(SUBJECT, x1 , x2)
#' # Compound symmetry structure
#' mc_mixed(~0 + SUBJECT, data = data)
#' # Compound symmetry + random slope for x1 and interaction or correlation
#' mc_mixed(~0 + SUBJECT/x1, data = data)
#' # Compound symmetry + random slope for x1 and x2 plus interactions
#' mc_mixed(~0 + SUBJECT/(x1 + x2), data = data)
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @source Bonat, et. al. (2016). Modelling the covariance structure in
#' marginal multivariate count models: Hunting in Bioko Island.
#' Environmetrics, submitted.
#'
#' @seealso \code{mc_id}, \code{mc_conditional_test},
#'  \code{mc_dist}, \code{mc_ma}, \code{mc_rw} and \code{mc_car}.
#'
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
