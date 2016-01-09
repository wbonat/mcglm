print("Testing link functions ...")
library(mcglm)  ## devtools::load_all("../../")
list.link <- list("logit", "probit","cauchit","cloglog", "loglog",
                  "identity","log","sqrt","1/mu^2","inverse")

test_that("length of output, dimensions of mu and D.",
{
    ## Build elements.
    x2 <- c(1, 1, 1, 0, 0)
    x1 <- seq(0, 1, length.out = length(x2))
    X <- model.matrix(~ x1 + x2)
    L <- mapply(FUN = mc_link_function,
                list.link,
                MoreArgs = list(beta = c(0.1, 0.2, 0.3), X = X,
                                offset = NULL),
                SIMPLIFY = FALSE)
    ## Test the length of output.
    expected <- 2L
    actual <- lapply(L, FUN = length)
    output <- lapply(actual, function(x) { x - expected }) != 0
    if (any(output)) {
        message(paste0(
            "Error: Problems on length of link function output: ",
            paste(unlist(list.link[output]), collapse = ", "), "."))
    }
    ## Test the length of mu vector.
    expected <- length(x1)
    actual <- lapply(lapply(L, FUN = "[[", "mu"), FUN = length)
    output <- lapply(actual, function(x) { x - expected }) != 0
    if (any(output)) {
        message(paste0(
            "Error: Problems on length of mu vector output: ",
            paste(unlist(list.link[output]), collapse = ", "), "."))
    }
    ## Test the dimension of D matrix.
    expected <- dim(X)
    actual <- lapply(lapply(L, FUN = "[[", "D"), FUN = dim)
    output <- lapply(actual, function(x) { sum(x - expected) }) != 0
    if (any(output)) {
        message(paste0(
            "Error: Problems on dimension of D matrix: ",
            paste(unlist(list.link[output]), collapse = ", "), "."))
    }
})

test_that("NA's on beta argument",
{
    x2 <- c(1, 1, 1, 0, 0)
    x1 <- seq(0, 1, length.out = length(x2))
    X <- model.matrix(~ x1 + x2)
    expect_error(mc_link_function(beta = c(0.1, 0.2, NA), X = X,
                                  offset = NULL, link = "log"))
})

test_that("NA's on X design matrix argument",
{
    x2 <- c(1, 1, 1, 0, 0)
    x1 <- seq(0, 1, length.out = length(x2))
    X <- model.matrix(~ x1 + x2)
    X[1, 2] <- NA
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL, link = "log"))
})

test_that("NA's on offset argument",
{
    x2 <- c(1, 1, 1, 0, 0)
    x1 <- seq(0, 1, length.out = length(x2))
    X <- model.matrix(~ x1 + x2)
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = c(NA, 2, 3, NA, NA),
                                  link = "log"))
})

test_that("String on mu argument",
{
    x2 <- c(1, 1, 1, 0, 0)
    x1 <- seq(0, 1, length.out = length(x2))
    X <- model.matrix(~ x1 + x2)
    expect_error(mc_link_function(beta = c(0.1, 0.2, "a"), X = X,
                                  offset = c(NA, 2, 3, NA, NA),
                                  link = "log"))
})

test_that("Test arguments of a user defined link function",
{
    x2 <- c(1, 1, 1, 0, 0)
    x1 <- seq(0, 1, length.out = length(x2))
    X <- model.matrix(~ x1 + x2)
    ## A user defined link function (udlf).
    ## Test on argument names.
    udlf <- function(a, X, offset) { NULL }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
    ## Test for non needed arguments.
    udlf <- function(beta, X, offset, ...){ NULL }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
    ## Test for omisson of offset (or another argument).
    udlf <- function(beta, X){ NULL }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
})

test_that("Test returned values of a user defined link function",
{
    x2 <- c(1, 1, 1, 0, 0)
    x1 <- seq(0, 1, length.out = length(x2))
    X <- model.matrix(~ x1 + x2)
    ## A user defined link function (udlf).
    ## Test the class or return value.
    udlf <- function(beta, X, offset) { NULL }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
    ## Test the names of returned value.
    udlf <- function(beta, X, offset) {
        list(A = NA, B = NA)
    }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
    ## Test on vector mu.
    udlf <- function(beta, X, offset) {
        list(mu = 1, D = X)
    }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
    ## Test on D dimentions.
    udlf <- function(beta, X, offset) {
        list(mu = x1, D = X[-1,])
    }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
    ## Test if was provided a data.frame.
    udlf <- function(beta, X, offset) {
        list(mu = x1, D = as.data.frame(X))
    }
    expect_error(mc_link_function(beta = c(0.1, 0.2, 0.3), X = X,
                                  offset = NULL,
                                  link = "udlf"))
})

print("Testing link functions ... OK")
