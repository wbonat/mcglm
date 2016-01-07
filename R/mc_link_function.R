#' Link functions
#'
#' @description  The \code{mc_link_function} is a customized call of the
#' \code{\link[stats]{make.link}} function.
#' 
#' Given the name of a link function, it returns a list with two
#' elements.  The first element is the inverse of the link function
#' applied on the linear predictor \eqn{\mu = g^{-1}(X\beta).} The
#' second element is the derivative of mu with respect to the regression
#' parameters \eqn{\beta}. It will be useful when computing the
#' quasi-score function.
#'
#' @param beta A numeric vector of regression parameters.
#' @param X A design matrix, see \code{\link[stats]{model.matrix}} for
#'     details.
#' @param offset A numeric vector of offset values. It will be sum up on
#'     the linear predictor as a covariate with known regression
#'     parameter equals one (\eqn{\mu = g^{-1}(X\beta + offset)}).  If
#'     no offset is present in the model, set offset = NULL.
#' @param link A string specifing the name of the link function. mcglm
#'     implements the following link functions: logit, probit, cauchit,
#'     cloglog, loglog, identity, log, sqrt, 1/mu^2 and inverse.
#' @return A list with two elements: mu and D.
#' @seealso \code{\link[stats]{model.matrix}},
#'     \code{\link[stats]{make.link}}.
#' @details The link function is an important component of the
#'     multivariate covariance generalized linear model, since it link
#'     the expectation of the response variable with the covariates.
#'     Let \eqn{\beta} a \eqn{p x 1} regression parameter vector and
#'     \eqn{X} an \eqn{n x p} design matrix. The expected value of a
#'     response variable \eqn{Y} is given by \deqn{E(Y) =
#'     g^{-1}(X\beta),} where \eqn{g} is the link function and \eqn{\eta
#'     = X\beta} is the linear predictor. Let \eqn{D} be a \eqn{n \times
#'     p} matrix whose entries are given by the derivatives of \eqn{mu}
#'     with respect to \eqn{\beta}.  Such matrix will be required by the
#'     fitting algorithm. The function \code{mc_link_function} returns a
#'     list where the first element is mu (n x 1) vector and the second
#'     D (n x p) matrix.
#' @examples
#' x1 <- seq(-1, 1, l = 5)
#' X <- model.matrix(~ x1)
#' mc_link_function(beta = c(1,0.5), X = X,
#'                  offset = NULL, link = 'log')
#' mc_link_function(beta = c(1,0.5), X = X,
#'                  offset = rep(10,5), link = 'identity')
#' @export
#' @import assertthat

## Generic link function -----------------------------------------------
mc_link_function <- function(beta, X, offset, link) {
    assert_that(noNA(beta))
    assert_that(noNA(X))
    if (!is.null(offset)) 
        assert_that(noNA(offset))
    link_name <- c("logit", "probit", "cauchit", "cloglog", "loglog",
                   "identity", "log", "sqrt", "1/mu^2", "inverse")
    link_func <- c("mc_logit", "mc_probit", "mc_cauchit", "mc_cloglog",
                   "mc_loglog", "mc_identity", "mc_log", "mc_sqrt",
                   "mc_invmu2", "mc_inverse")
    names(link_func) <- link_name
    if (!link %in% link_name) {
        stop(gettextf(paste0("%s link not recognised. ",
                             "Available links are: ",
                             paste(link_name, collapse = ", "),
                             "."),
                      sQuote(link)), domain = NA)
    }
    output <- do.call(link_func[link],
                      args = list(beta = beta, X = X, offset = offset))
    return(output)
}

#' @rdname mc_link_function
## Logit link function -------------------------------------------------
mc_logit <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu <- make.link("logit")$linkinv(eta = eta)
    return(list(mu = mu, D = X * (mu * (1 - mu))))
}

#' @rdname mc_link_function
## Probit link function ------------------------------------------------
mc_probit <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu <- make.link("probit")$linkinv(eta = eta)
    Deri <- make.link("probit")$mu.eta(eta = eta)
    return(list(mu = mu, D = X * Deri))
}

#' @rdname mc_link_function
## Cauchit link function -----------------------------------------------
mc_cauchit <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu = make.link("cauchit")$linkinv(eta = eta)
    Deri <- make.link("cauchit")$mu.eta(eta = eta)
    return(list(mu = mu, D = X * Deri))
}

#' @rdname mc_link_function
## Complement log-log link function ------------------------------------
mc_cloglog <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu = make.link("cloglog")$linkinv(eta = eta)
    Deri <- make.link("cloglog")$mu.eta(eta = eta)
    return(list(mu = mu, D = X * Deri))
}

#' @rdname mc_link_function
## Log-log link function -----------------------------------------------
mc_loglog <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu <- exp(-exp(-eta))
    Deri <- exp(-exp(-eta) - eta)
    return(list(mu = mu, D = X * Deri))
}

#' @rdname mc_link_function
## Identity link function ----------------------------------------------
mc_identity <- function(beta, X, offset) {
    eta <- X %*% beta
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    return(list(mu = as.numeric(eta), D = X))
}

#' @rdname mc_link_function
## Log link function ---------------------------------------------------
mc_log <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu = make.link("log")$linkinv(eta = eta)
    return(list(mu = mu, D = X * mu))
}

#' @rdname mc_link_function
## Square-root link function -------------------------------------------
mc_sqrt <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu = make.link("sqrt")$linkinv(eta = eta)
    return(list(mu = mu, D = X * (2 * as.numeric(eta))))
}

#' @rdname mc_link_function
## Inverse mu square link function -------------------------------------
mc_invmu2 <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu <- make.link("1/mu^2")$linkinv(eta = eta)
    Deri <- make.link("1/mu^2")$mu.eta(eta = eta)
    return(list(mu = mu, D = X * Deri))
} 

#' @rdname mc_link_function
## Inverse link function -----------------------------------------------
mc_inverse <- function(beta, X, offset) {
    eta <- as.numeric(X %*% beta)
    if (!is.null(offset)) {
        eta <- eta + offset
    }
    mu <- make.link("inverse")$linkinv(eta = eta)
    Deri <- make.link("inverse")$mu.eta(eta = eta)
    return(list(mu = mu, D = X * Deri))
}
