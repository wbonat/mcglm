print("test_mc_build_sigma.R")
## Tests mc_build_sigma
x1 <- seq(0,1,l=10)
X <- model.matrix(~ x1)
mu <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "logit")
Z0 <- Diagonal(10,1)
Z1 <- Matrix(tcrossprod(rep(1,10)))
Z2 <- Matrix(c(rep(0,5),rep(1,5))%*%t(c(rep(0,5),rep(1,5))))
Z <- list(Z0,Z1,Z2)

test_that(
  "Covariance function: identity | variance function: constant",
  {
    Sigma <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    inv_Sigma <- solve(Sigma)
    expect1 <- 3
    expect2 <- length(Z)
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = NULL, Z = Z, sparse = FALSE,
                             variance = "constant", covariance = "identity", power_fixed = FALSE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(Z[[1]]), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(Z[[2]]), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(Z[[3]]), as.matrix(actual$D_Sigma[[3]]))
    }
)

test_that(
  "Covariance function: inverse | variance function: constant",
  {
    inv_Sigma <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    Sigma <- solve(inv_Sigma)
    expect1 <- 3
    expect2 <- length(Z)
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = NULL, Z = Z, sparse = FALSE,
                             variance = "constant", covariance = "inverse", power_fixed = FALSE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%(actual$Sigma_chol)))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(-Sigma%*%Z[[1]]%*%Sigma), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(-Sigma%*%Z[[2]]%*%Sigma), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(-Sigma%*%Z[[3]]%*%Sigma), as.matrix(actual$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: expm | variance function: constant",
  {
    U <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    D_Sigma1 <- mc_dexp_gold(M = U, dM = Z[[1]])
    D_Sigma2 <- mc_dexp_gold(M = U, dM = Z[[2]])
    D_Sigma3 <- mc_dexp_gold(M = U, dM = Z[[3]])
    Sigma <- D_Sigma1[[1]]
    inv_Sigma <- solve(Sigma)
    expect1 <- 3
    expect2 <- length(Z)
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = NULL, Z = Z, sparse = FALSE,
                             variance = "constant", covariance = "expm", power_fixed = FALSE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma1[[2]]), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma2[[2]]), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma3[[2]]), as.matrix(actual$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: identity | variance function: tweedie",
  {
    Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    V <- mc_variance_function(mu = mu$mu, power = 2, Ntrial = NULL, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%Z[[1]]%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%Z[[2]]%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%Z[[3]]%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             variance = "tweedie", covariance = "identity", power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             variance = "tweedie", covariance = "identity", power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%(actual$Sigma_chol)))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))

  }
)

test_that(
  "Covariance function: inverse | variance function: tweedie",
  {
    inv_Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    Omega <- solve(inv_Omega)
    V <- mc_variance_function(mu = mu$mu, power = 2, Ntrial = NULL, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%(-Omega%*%Z[[1]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(-Omega%*%Z[[2]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(-Omega%*%Z[[3]]%*%Omega)%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             variance = "tweedie", covariance = "inverse", power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             variance = "tweedie", covariance = "inverse", power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%(actual$Sigma_chol)))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: expm | variance function: tweedie",
  {
    U <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z[[1]])
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z[[2]])
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z[[3]])
    Omega <- D_Omega1[[1]]
    V <- mc_variance_function(mu = mu$mu, power = 2, Ntrial = NULL, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%(D_Omega1[[2]])%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(D_Omega2[[2]])%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(D_Omega3[[2]])%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             variance = "tweedie", covariance = "expm", power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                              variance = "tweedie", covariance = "expm", power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

mu <- mc_link_function(beta = c(1,0.5), X = X, offset = NULL, link = "logit")
test_that(
  "Covariance function: identity | variance function: binomialP",
  {
    Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    V <- mc_variance_function(mu = mu$mu, power = 2, Ntrial = 1, variance = "binomialP",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%Z[[1]]%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%Z[[2]]%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%Z[[3]]%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             Ntrial=1, variance = "binomialP", covariance = "identity",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "binomialP", covariance = "identity",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: inverse | variance function: binomialP",
  {
    inv_Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    Omega <- solve(inv_Omega)
    V <- mc_variance_function(mu = mu$mu, power = 2, Ntrial = 1, variance = "binomialP",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%(-Omega%*%Z[[1]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(-Omega%*%Z[[2]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(-Omega%*%Z[[3]]%*%Omega)%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             Ntrial = 1, variance = "binomialP", covariance = "inverse",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "binomialP", covariance = "inverse",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%(actual$Sigma_chol)))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: expm | variance function: binomialP",
  {
    U <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z[[1]])
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z[[2]])
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z[[3]])
    Omega <- D_Omega1[[1]]
    V <- mc_variance_function(mu = mu$mu, power = 2, Ntrial = 1, variance = "binomialP",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%(D_Omega1[[2]])%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(D_Omega2[[2]])%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(D_Omega3[[2]])%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                             Ntrial = 1, variance = "binomialP", covariance = "expm",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = 2, Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "binomialP", covariance = "expm",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

mu <- mc_link_function(beta = c(1,0.5), X = X, offset = NULL, link = "logit")
test_that(
  "Covariance function: identity | variance function: binomialPQ",
  {
    Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    V <- mc_variance_function(mu = mu$mu, power = c(2,1), Ntrial = 1, variance = "binomialPQ",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power1 <- V$D_V_sqrt_p%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_p
    D_Sigma_power2 <- V$D_V_sqrt_q%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_q
    D_Sigma_Z0 <- V$V_sqrt%*%Z[[1]]%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%Z[[2]]%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%Z[[3]]%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 2
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2,1), Z = Z, sparse = FALSE,
                             Ntrial=1, variance = "binomialPQ", covariance = "identity",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2,1), Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "binomialPQ", covariance = "identity",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power1), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_power2), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[4]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[5]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: inverse | variance function: binomialPQ",
  {
    inv_Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    Omega <- solve(inv_Omega)
    V <- mc_variance_function(mu = mu$mu, power = c(2,1), Ntrial = 1, variance = "binomialPQ",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power1 <- V$D_V_sqrt_p%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_p
    D_Sigma_power2 <- V$D_V_sqrt_q%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_q
    D_Sigma_Z0 <- V$V_sqrt%*%(-Omega%*%Z[[1]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(-Omega%*%Z[[2]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(-Omega%*%Z[[3]]%*%Omega)%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 2
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2,1), Z = Z, sparse = FALSE,
                             Ntrial = 1, variance = "binomialPQ", covariance = "inverse",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2,1), Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "binomialPQ", covariance = "inverse",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%(actual$Sigma_chol)))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power1), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_power2), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[4]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[5]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: expm | variance function: binomialPQ",
  {
    U <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z[[1]])
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z[[2]])
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z[[3]])
    Omega <- D_Omega1[[1]]
    V <- mc_variance_function(mu = mu$mu, power = c(2,1), Ntrial = 1, variance = "binomialPQ",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power1 <- V$D_V_sqrt_p%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_p
    D_Sigma_power2 <- V$D_V_sqrt_q%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_q
    D_Sigma_Z0 <- V$V_sqrt%*%(D_Omega1[[2]])%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(D_Omega2[[2]])%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(D_Omega3[[2]])%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 2
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2,1), Z = Z, sparse = FALSE,
                             Ntrial = 1, variance = "binomialPQ", covariance = "expm",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2,1), Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "binomialPQ", covariance = "expm",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power1), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_power2), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[4]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[5]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

mu <- mc_link_function(beta = c(1,0.5), X = X, offset = NULL, link = "log")
test_that(
  "Covariance function: identity | variance function: poisson_tweedie",
  {
    Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    V <- mc_variance_function(mu = mu$mu, power = c(2), Ntrial = 1, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- as(Diagonal(length(mu$mu), mu$mu) + V$V_sqrt%*%Omega%*%V$V_sqrt,"dsyMatrix")
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_power
    D_Sigma_Z0 <- V$V_sqrt%*%Z[[1]]%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%Z[[2]]%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%Z[[3]]%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2), Z = Z, sparse = FALSE,
                             Ntrial=NULL, variance = "poisson_tweedie", covariance = "identity",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2), Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "poisson_tweedie", covariance = "identity",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: inverse | variance function: poisson_tweedie",
  {
    inv_Omega <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    Omega <- solve(inv_Omega)
    V <- mc_variance_function(mu = mu$mu, power = c(2), Ntrial = 1, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- Diagonal(length(mu$mu), mu$mu) + V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_power
    D_Sigma_Z0 <- V$V_sqrt%*%(-Omega%*%Z[[1]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(-Omega%*%Z[[2]]%*%Omega)%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(-Omega%*%Z[[3]]%*%Omega)%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2), Z = Z, sparse = FALSE,
                             Ntrial = 1, variance = "poisson_tweedie", covariance = "inverse",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2), Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "poisson_tweedie", covariance = "inverse",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

test_that(
  "Covariance function: expm | variance function: poisson_tweedie",
  {
    U <- mc_matrix_linear_predictor(tau = c(2,0.8,0.5), Z = Z)
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z[[1]])
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z[[2]])
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z[[3]])
    Omega <- D_Omega1[[1]]
    V <- mc_variance_function(mu = mu$mu, power = c(2), Ntrial = 1, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- Diagonal(length(mu$mu), mu$mu) + V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_power
    D_Sigma_Z0 <- V$V_sqrt%*%(D_Omega1[[2]])%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(D_Omega2[[2]])%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(D_Omega3[[2]])%*%V$V_sqrt
    expect1 <- 3
    expect2 <- length(Z) + 1
    actual <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2), Z = Z, sparse = FALSE,
                             Ntrial = 1, variance = "poisson_tweedie", covariance = "expm",
                             power_fixed = FALSE)
    actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2), Z = Z, sparse = FALSE,
                              Ntrial = 1, variance = "poisson_tweedie", covariance = "expm",
                              power_fixed = TRUE)
    expect_equal(expect1, length(actual))
    expect_equal(expect2, length(actual$D_Sigma))
    expect_equal(expect1, length(actual2$D_Sigma))
    expect_equal(as.matrix(Sigma), as.matrix(t(actual$Sigma_chol)%*%actual$Sigma_chol))
    expect_equal(as.matrix(inv_Sigma), as.matrix((actual$Sigma_chol_inv)%*%t(actual$Sigma_chol_inv)))
    expect_equal(as.matrix(D_Sigma_power), as.matrix(actual$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual$D_Sigma[[3]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual$D_Sigma[[4]]))

    expect_equal(as.matrix(D_Sigma_Z0), as.matrix(actual2$D_Sigma[[1]]))
    expect_equal(as.matrix(D_Sigma_Z1), as.matrix(actual2$D_Sigma[[2]]))
    expect_equal(as.matrix(D_Sigma_Z2), as.matrix(actual2$D_Sigma[[3]]))
  }
)

#######################################################################################
## Computing the derivatives with respect to beta #####################################
#######################################################################################
#x1 <- seq(0,1,l=10)
#X <- model.matrix(~ x1)
#mu <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "logit")
#Z0 <- Diagonal(10,1)
#Z1 <- Matrix(tcrossprod(rep(1,10)))
#Z2 <- Matrix(c(rep(0,5),rep(1,5))%*%t(c(rep(0,5),rep(1,5))))
#Z <- list(Z0,Z1,Z2)

#actual2 <- mc_build_sigma(mu = mu, tau = c(2,0.8,0.5), power = c(2,1), Z = Z, sparse = FALSE,
#                          Ntrial = 1, variance = "binomialPQ", covariance = "expm",
#                          power_fixed = FALSE, compute_derivative_beta = TRUE)
#names(actual2)
#length(actual2$D_Sigma_beta)

print("test_mc_build_sigma.R - OK")
