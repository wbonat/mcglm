#' @title Link Functions
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description  The \code{mc_link_function} is a customized call of the
#' \code{\link[stats]{make.link}} function.
#'
#' Given the name of a link function, it returns a list with two
#' elements.  The first element is the inverse of the link function
#' applied on the linear predictor \eqn{\mu = g^{-1}(X\beta).} The
#' second element is the derivative of \eqn{\mu} with respect to the
#' regression parameters \eqn{\beta}.
#' It will be useful when computing the quasi-score function.
#'
#' @param beta a numeric vector of regression parameters.
#' @param X a design matrix, see \code{\link[stats]{model.matrix}} for
#'     details.
#' @param offset a numeric vector of offset values. It will be sum up on
#'     the linear predictor as a covariate with known regression
#'     parameter equals one (\eqn{\mu = g^{-1}(X\beta + offset)}).  If
#'     no offset is present in the model, set offset = NULL.
#' @param link a string specifying the name of the link function.
#'     Options are: \code{"logit"}, \code{"probit"}, \code{"cauchit"},
#'     \code{"cloglog"}, \code{"loglog"}, \code{"identity"}, \code{"log"},
#'     \code{"sqrt"}, \code{"1/mu^2"} and \code{inverse}.
#'     A user defined link function can be used (see Details).
#'
#' @return A list with two elements: mu and D (see Details).
#'
#' @seealso \code{\link[stats]{model.matrix}},
#'     \code{\link[stats]{make.link}}.
#'
#' @details The link function is an important component of the
#'     multivariate covariance generalized linear models, since it links
#'     the expectation of the response variable with the covariates.
#'     Let \eqn{\beta} be a (p x 1) regression parameter vector and
#'     \eqn{X} be an (n x p) design matrix. The expected value of
#'     the response variable \eqn{Y} is given by \deqn{E(Y) =
#'     g^{-1}(X\beta),} where \eqn{g} is the link function and \eqn{\eta
#'     = X\beta} is the linear predictor. Let \eqn{D} be a (n x p)
#'     matrix whose entries are given by the derivatives of \eqn{\mu}
#'     with respect to \eqn{\beta}.  Such a matrix will be required for the
#'     fitting algorithm. The function \code{mc_link_function} returns a
#'     list where the first element is \eqn{\mu} (n x 1) vector
#'     and the second is the D (n x p) matrix.
#'     A user defined function can also be used. It must be a function
#'     with arguments \code{beta}, \code{X} and \code{offset}
#'     (set to \code{NULL} if non needed). The function must return a
#'     length 2 named list with \code{mu} and \code{D} elements as a
#'     vector and a matrix of proper dimensions.
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
        ## Test if link function exists outside.
        if (!exists(link, envir = -1, mode = "function")) {
        stop(gettextf(paste0(
            "%s link function not recognised or found. ",
            "Available links are: ",
            paste(link_name, collapse = ", "),
            "."),
            sQuote(link)), domain = NA)
        } else {
            match_args <- sort(names(formals(link))) %in%
                sort(c("beta", "X", "offset"))
            ## Test if provided funtion has correct arguments.
            if (length(match_args) != 3L || !all(match_args)) {
                stop(gettextf(paste(
                    "Provided link function must have %s, %s and %s",
                    "as arguments to be valid."),
                    sQuote("beta"), sQuote("X"), sQuote("offset")),
                    domain = NA)
            }
        }
        output <- do.call(link,
                          args = list(beta = beta, X = X,
                                      offset = offset))
        if (!is.list(output)) {
            stop("Provided link funtion doesn't return a list.")
        }
        if (!identical(sort(names(output)), c("D","mu"))) {
            stop(paste0("Provided link funtion isn't return ",
                        "a list with names ", sQuote("mu"),
                        " and ", sQuote("D"), "."))
        }
        if (!(identical(dim(output$D), dim(X)) &&
              is.matrix(output$D))) {
            stop(paste0("Returned ", sQuote("D"),
                        " object by user defined link function ",
                        "isn't a matrix of correct dimensions."))
        }
        print(is.vector(output$mu, mode = "vector"))
        print(class(output$mu))
        if (!(length(output$mu) == nrow(X) &&
                  is.vector(output$mu, mode = "numeric"))) {
            stop(paste0("Returned ", sQuote("mu"),
                        " object by user defined link function ",
                        "isn't a vector of correct length."))
            is.vector(output$mu, mode = "vector")
        }
    } else {
        link <- link_func[link]
        output <- do.call(link,
                          args = list(beta = beta, X = X,
                                      offset = offset))
    }
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
