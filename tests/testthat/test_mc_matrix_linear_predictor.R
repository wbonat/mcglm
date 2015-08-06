print("Testing Matrix linear predictor...")
library(mcglm)
test_that(
  "wrong number of parameters",
  {
    Z0 <- Diagonal(5, 1)
    Z1 <- Matrix(rep(1,5)%*%t(rep(1,5)))
    Z <- list(Z0, Z1)
    expect_error(mc_matrix_linear_predictor(tau = c(1), Z = Z))
  }
)

test_that(
  "Checking the result",
  {
    Z0 <- Diagonal(5, 1)
    Z1 <- Matrix(rep(1,5)%*%t(rep(1,5)))
    expect <- Z0 + 0.8*Z1
    Z <- list(Z0, Z1)
    actual <- mc_matrix_linear_predictor(tau = c(1,0.8), Z = Z)
    expect_equal(actual,expect)
  }
)

print("Testing Matrix linear predictor... OK")
