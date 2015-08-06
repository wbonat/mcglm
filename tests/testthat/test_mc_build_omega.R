###############################################################
# Test - mc_build_omega #######################################
###############################################################
require(mcglm)
Z0 <- Diagonal(10,1)
Z1 <- Matrix(tcrossprod(rep(1,10)))
Z2 <- Matrix(c(rep(0,5),rep(1,5))%*%t(c(rep(0,5),rep(1,5))))
Z <- list(Z0,Z1,Z2)

###############################################################
## Identity covariance link function ##########################
###############################################################
test_that(
  "Length of output - Identity",
  {
    expect1 <- 2
    expect2 <- length(Z)
    actual <- mc_build_omega(tau = c(2,0.8,0.5), Z = Z, covariance = "identity", sparse = FALSE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Omega))
  }
)
###############################################################
## Identity covariance link function ##########################
###############################################################
test_that(
  "Length of output - Inverse",
  {
    expect1 <- 2
    expect2 <- length(Z)
    actual <- mc_build_omega(tau = c(2,0.8,0.5), Z = Z, covariance = "inverse", sparse = FALSE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_inv_Omega))
  }
)


###############################################################
## Exponential-matrix covariance link function ################
###############################################################
test_that(
  "Length of output - Exponential-matrix",
  {
    expect1 <- 2
    expect2 <- length(Z)
    actual <- mc_build_omega(tau = c(2, 0.8, 0.5), Z = Z, covariance = "expm", sparse = FALSE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Omega))
  }
)

test_that(
  "Derivatives check - Exponential-matrix",
  {
    U <- mc_matrix_linear_predictor(tau = c(2,0.1,0.01), Z = Z)
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z[[1]])
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z[[2]])
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z[[3]])
    Omega <- D_Omega1[[1]]
    actual <- mc_build_omega(tau = c(2,0.1,0.01), Z = Z, covariance = "expm", sparse = FALSE)
    expect_equal(as.matrix(Omega), as.matrix(actual$Omega))
    expect_equal(as.matrix(D_Omega1[[2]]), as.matrix(actual$D_Omega[[1]]))
    expect_equal(as.matrix(D_Omega2[[2]]), as.matrix(actual$D_Omega[[2]]))
    expect_equal(as.matrix(D_Omega3[[2]]), as.matrix(actual$D_Omega[[3]]))
    }
)
