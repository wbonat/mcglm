print("Testing variance function ...")
library(mcglm)
x1 <- seq(-1,1, l = 5)
X <- model.matrix(~ x1)
mu_positive <- mc_link_function(beta = c(1,0.3), X = X, offset = NULL, link = "log")

## General tests to avoid wrong enters
test_that(
  "wrong arguments - factor instead of logical",
  {
    expect_error(mc_variance_function(mu = mu_positive$mu, power = 2, variance = "power",
                                      inverse = "a", derivative_power = FALSE,
                                      derivative_mu = FALSE))
  }
)

test_that(
  "wrong arguments - factor instead of logical",
  {
    expect_error(mc_variance_function(mu = mu_positive$mu, power = 2, variance = "power",
                                      inverse = TRUE, derivative_power = "a",
                                      derivative_mu = FALSE))
  }
)

test_that(
  "wrong names",
  {
    expect_error(mc_variance_function(mu = mu_positive$mu, power = 2, variance = "Power",
                                      inverse = TRUE, derivative_power = FALSE,
                                      derivative_mu = FALSE))
  }
)


# Power variance function -------------------------------------
test_that(
  "length of output - power variance function",
  {
    expected <- 1
    actual <- length(mc_variance_function(mu = mu_positive$mu, power = 2, inverse = FALSE,
                       derivative_power = FALSE, derivative_mu = FALSE, variance = "power"))
    expect_equal(actual, expected)
    expected <- 2
    actual <- length(mc_variance_function(mu = mu_positive$mu, power = 2, inverse = FALSE,
                        derivative_power = TRUE, derivative_mu = FALSE, variance = "power"))
    expect_equal(actual, expected)
    expected <- 3
    actual <- length(mc_variance_function(mu = mu_positive$mu, power = 2, inverse = FALSE,
                        derivative_power = TRUE, derivative_mu = TRUE, variance = "power"))
    expect_equal(actual, expected)
  }
)

test_that(
  "wrong length of power parameter - power variance function",
  {
  expect_error(mc_variance_function(mu = mu_positive$mu, power = c(2,1), inverse = FALSE,
                  derivative_power = FALSE, derivative_mu = FALSE, variance = "power"))
  }
)

test_that(
  "negative values on mu - power parameter",
  {
    mu_positive$mu[3] <- -1
    expect_error(mc_variance_function(mu = mu_positive$mu, power = 2, inverse = FALSE,
                    derivative_power = FALSE, derivative_mu = FALSE, variance = "power"))
  }
)

# END Power variance function -------------------------------------

# BinomialP variance function -------------------------------------
mu_binomial <- mc_link_function(beta = c(1,0.3), X = X, offset = NULL, link = "logit")
test_that(
  "length of output - binomialP variance functions",
  {
    expected <- 1
    actual <- length(mc_variance_function(mu = mu_binomial$mu, power = 2, inverse = FALSE,
                                          derivative_power = FALSE, derivative_mu = FALSE,
                                          Ntrial = rep(10,5), variance = "binomialP"))
    expect_equal(actual, expected)
    expected <- 2
    actual <- length(mc_variance_function(mu = mu_binomial$mu, power = 2, inverse = FALSE,
                                          derivative_power = TRUE, derivative_mu = FALSE,
                                          Ntrial = rep(10,5), variance = "binomialP"))
    expect_equal(actual, expected)
    expected <- 3
    actual <- length(mc_variance_function(mu = mu_binomial$mu, power = 2, inverse = FALSE,
                                          derivative_power = TRUE, derivative_mu = TRUE,
                                          Ntrial = rep(10,5), variance = "binomialP"))
    expect_equal(actual, expected)
  }
)

test_that(
  "wrong length of power parameter - binomialP variance function",
  {
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = c(2,1), inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialP"))
  }
)

test_that(
  "negative values on mu - binomialP variance function",
  {
    mu_binomial$mu[3] <- -1
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = 2, inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialP"))
    }
)

test_that(
  "bigger than on values on mu - binomialP variance function",
  {
    mu_binomial$mu[3] <- 1.2
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = 2, inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialP"))
  }
)

test_that(
  "exact zero on values on mu - binomialP variance function",
  {
    mu_binomial$mu[3] <- 0
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = 2, inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialP"))
  }
)

# END binomialP ------------------------------

# BinomialPQ variance function -------------------------------------
mu_binomial <- mc_link_function(beta = c(1,0.3), X = X, offset = NULL, link = "logit")
test_that(
  "length of output - binomialPQ variance functions",
  {
    expected <- 1
    actual <- length(mc_variance_function(mu = mu_binomial$mu, power = c(2,1), inverse = FALSE,
                                          derivative_power = FALSE, derivative_mu = FALSE,
                                          Ntrial = rep(10,5), variance = "binomialPQ"))
    expect_equal(actual, expected)
    expected <- 3
    actual <- length(mc_variance_function(mu = mu_binomial$mu, power = c(2,1), inverse = FALSE,
                                          derivative_power = TRUE, derivative_mu = FALSE,
                                          Ntrial = rep(10,5), variance = "binomialPQ"))
    expect_equal(actual, expected)
    expected <- 4
    actual <- length(mc_variance_function(mu = mu_binomial$mu, power = c(2,1), inverse = FALSE,
                                          derivative_power = TRUE, derivative_mu = TRUE,
                                          Ntrial = rep(10,5), variance = "binomialPQ"))
    expect_equal(actual, expected)
  }
)

test_that(
  "wrong length of power parameter - binomialPQ variance function",
  {
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = c(2), inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialPQ"))
  }
)

test_that(
  "negative values on mu - binomialP variance function",
  {
    mu_binomial$mu[3] <- -1
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = c(2,1), inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialPQ"))
  }
)

test_that(
  "bigger than one values on mu - binomialPQ variance function",
  {
    mu_binomial$mu[3] <- 1.2
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = c(2,1), inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialPQ"))
  }
)

test_that(
  "exact zero - binomialPQ variance function",
  {
    mu_binomial$mu[3] <- 0
    expect_error(mc_variance_function(mu = mu_binomial$mu, power = c(2,1), inverse = FALSE,
                                      derivative_power = FALSE, derivative_mu = FALSE,
                                      Ntrial = rep(10,5), variance = "binomialPQ"))
  }
)

# END binomialPQ ------------------------------
print("Testing variance function ... OK")
