print("Testing link functions ...")
library(mcglm)
list.link <- list("logit", "probit","cauchit","cloglog",
                  "loglog","identity","log","sqrt","1/mu^2","inverse")

test_that(
  "length of output",
  {
    expected <- 2
    x1 <- seq(0,1,l=5)
    x2 <- c(1,1,1,0,0)
    X <- model.matrix(~ x1 + x2)
    actual <- lapply(lapply(list.link, function(x)
                     mc_link_function(beta = c(0.1,0.2,0.3), X = X, offset = NULL, link = x)),length)
    nomes <- unlist(list.link)
    output <- which(lapply(actual, function(x) x - expected) != 0)
    if(length(output) != 0)message(
      paste("Error: Problems on length of link function output:",nomes[output]))

  }
)

test_that(
  "length of mu vector",
  {
    expected <- 5
    x1 <- seq(0,1,l=5)
    x2 <- c(1,1,1,0,0)
    X <- model.matrix(~ x1 + x2)
    actual <- lapply(lapply(list.link, function(x)
      mc_link_function(beta = c(0.1,0.2,0.3), X = X, offset = NULL, link = x)$mu),length)
    nomes <- unlist(list.link)
    output <- which(lapply(actual, function(x) x - expected) != 0)
    if(length(ouptut) != 0)message(
      paste("Error: Problems on length of mu vector output:",nomes[output]))

  }
)

test_that(
  "dimension of D matrix",
  {
    expected <- c(5,3)
    x1 <- seq(0,1,l=5)
    x2 <- c(1,1,1,0,0)
    X <- model.matrix(~ x1 + x2)
    actual <- lapply(lapply(list.link, function(x)
      mc_link_function(beta = c(0.1,0.2,0.3), X = X, offset = NULL, link = x)$D),dim)
    nomes <- unlist(list.link)
    output <- which(lapply(actual, function(x) sum(x - expected)) != 0)
    if(length(output) != 0)message(
      paste("Error: Problems on dimension of D matrix:",nomes[output]))
  }
)

test_that(
  "NA's on beta argument",
  {
    x1 <- seq(0,1,l=5)
    x2 <- c(1,1,1,0,0)
    X <- model.matrix(~ x1 + x2)
    expect_error(mc_link_function(beta = c(0.1,0.2, NA), X = X, offset = NULL, link = "log"))
  }
)

test_that(
  "NA's on X design matrix argument",
  {
    x1 <- seq(0,1,l=5)
    x2 <- c(1,1,1,0,0)
    X <- model.matrix(~ x1 + x2)
    X[1,2] <- NA
    expect_error(mc_link_function(beta = c(0.1,0.2, 0.3), X = X, offset = NULL, link = "log"))
  }
)

test_that(
  "NA's on offset argument",
  {
    x1 <- seq(0,1,l=5)
    x2 <- c(1,1,1,0,0)
    X <- model.matrix(~ x1 + x2)
    expect_error(mc_link_function(beta = c(0.1,0.2, 0.3), X = X, offset = c(NA,2,3,NA,NA),
                                  link = "log"))
  }
)

test_that(
  "String on mu argument",
  {
    x1 <- seq(0,1,l=5)
    x2 <- c(1,1,1,0,0)
    X <- model.matrix(~ x1 + x2)
    expect_error(mc_link_function(beta = c(0.1,0.2, "a"), X = X, offset = c(NA,2,3,NA,NA),
                                  link = "log"))
  }
)
print("Testing link functions ... OK")
