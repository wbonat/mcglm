print("Running test_mc_build_C.R")

## Auxiliar function
expect_smaller <- function(a, tol){
  if(a > tol) stop("Error: Elements of your input are not smaller than tolerance!")
}

## Tests mc_build_C matrix
library(mcglm)

x1 <- seq(-1,1, l=10)
X <- model.matrix(~x1)
mu1 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "logit")

## Creating some matrices
Z0 <- Diagonal(10, 1)
Z1 <- Matrix(rep(1,10)%*%t(rep(1,10)))
Z2 <- Matrix(c(rep(0,5),rep(1,5))%*%t(c(rep(0,5),rep(1,5))))

## Arguments
list_mu <- list("resp1" = mu1)
list_Ntrial <- list("resp1" = rep(1,10))
list_tau <- list("resp1" = c(1,0.1,0.2))
list_power <- list("resp1" = c(2))
list_Z <- list("resp1" = list(Z0,Z1,Z2))
list_sparse = list("resp1" = FALSE)
list_variance = list("resp1" ="constant")
list_covariance = list("resp1" = "identity")
list_power_fixed = list("resp1" = FALSE)

name.covariance <- c("identity","inverse","expm")
for(i in 1:3){
  list_covariance <- list("resp1" = name.covariance[i])
  print(paste("Testing covariance", name.covariance[i]))
### Tests - Univariate case
test_that(
  "length of output - Identity + constant",
  {
    expected1 <- 2
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial, rho = c(0.12),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(length(actual), expected1)
    expect_equal(length(actual$D_C), 3)
  }
)


test_that(
  "length of output - Identity + tweedie, poisson_tweedie, binomialP",
  {
    name <- c("tweedie", "poisson_tweedie","binomialP")
    for(i in 1:3){
    list_variance <- list("resp1" = name[i])
    expected1 <- 2
    expected2 <- 4
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial, rho = c(0.12),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(length(actual), expected1)
    expect_equal(length(actual$D_C), expected2)
    }
  }
)

test_that(
  "length of output - binomialPQ",
  {
      list_variance <- list("resp1" = "binomialPQ")
      list_power <- list("resp1" = c(2,1))
      expected1 <- 2
      expected2 <- 5
      actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial, rho = c(0.12),
                          list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                          list_sparse = list_sparse, list_variance = list_variance,
                          list_covariance = list_covariance, list_power_fixed = list_power_fixed)
      expect_equal(length(actual), expected1)
      expect_equal(length(actual$D_C), expected2)
    }
)
print("OK")
}

### Five response variables
mu1 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "logit")
mu2 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "probit")
mu3 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "cloglog")
mu4 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "cauchit")
mu5 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "loglog")
## Creating some matrices
Z0 <- Diagonal(10, 1)
Z1 <- Matrix(rep(1,10)%*%t(rep(1,10)))
Z2 <- Matrix(c(rep(0,5),rep(1,5))%*%t(c(rep(0,5),rep(1,5))))
Z3 <- Matrix(c(rep(0,3),rep(1,3),rep(2,4))%*%t(c(rep(0,3),rep(1,3),rep(2,4))))
Z4 <- forceSymmetric(Matrix(rnorm(100),10,10))


## Arguments
list_mu <- list("resp1" = mu1, "resp2" = mu2,"resp3" = mu3,
                "resp4" = mu4,"resp5" = mu5)
list_Ntrial <- list("resp1" = rep(1,10),"resp2" = rep(1,10),"resp3" = rep(1,10),
                    "resp4" = rep(1,10),"resp5" = rep(1,10))
list_tau <- list("resp1" = c(1),"resp2" = c(1,0.8),"resp3" = c(2,0.8,0.3),
                 "resp4" = c(2,0.8,0.3,-0.2),"resp5" = c(2,0.8,0.3,-0.2,0))
list_power <- list("resp1" = c(1), "resp2" = c(2),"resp3" = c(3),"resp4" = c(1),"resp5" = c(2,1))
list_Z <- list("resp1" = list(Z0),"resp2" = list(Z0,Z1),"resp3" = list(Z0,Z2,Z3),
               "resp4" = list(Z0,Z1,Z2,Z3),"resp5" = list(Z0,Z1,Z2,Z3,Z4))
list_sparse = list("resp1" = FALSE,"resp2" = FALSE,"resp3" = FALSE,"resp4" = FALSE,"resp5" = FALSE)
list_variance = list("resp1" ="constant", "resp2" = "tweedie", "resp3" = "poisson_tweedie",
                     "resp4" = "binomialP", "resp5" = "binomialPQ")
list_covariance = list("resp1" = "identity", "resp2" = "inverse","resp3" = "expm",
                       "resp1" = "identity","resp1" = "inverse")
list_power_fixed = list("resp1" = FALSE, "resp2" = FALSE, "resp3" = FALSE,
                        "resp4" = FALSE, "resp5" = FALSE)

test_that(
    "length of output",
    {
      expected1 <- 2
      expected2 <- 30
      actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                          rho = c(0.12, 0.13,0.14,0.15,0.23,0.24,0.25,0.34,0.35,0.45),
                          list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                          list_sparse = list_sparse, list_variance = list_variance,
                          list_covariance = list_covariance, list_power_fixed = list_power_fixed)
      expect_equal(length(actual), expected1)
      expect_equal(length(actual$D_C), expected2)
    }
  )

print("Checking computations")
#####################################################################################
## Checking all calculations ########################################################
#####################################################################################
list_mu <- list("resp1" = mu1)
list_Ntrial <- list("resp1" = rep(1,10))
list_tau <- list("resp1" = c(1,0.1,0.2))
list_power <- list("resp1" = c(2))
list_Z <- list("resp1" = list(Z0,Z1,Z2))
list_sparse = list("resp1" = FALSE)
list_variance = list("resp1" ="constant")
list_covariance = list("resp1" = "identity")
list_power_fixed = list("resp1" = FALSE)

######################################################################################
### Constant variance function #######################################################
######################################################################################
test_that(
  "Checking derivatives - Univariate case + identity",
  {
  Sigma <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
  inv_Sigma <- solve(Sigma)
  actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(1),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
  expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
  expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
  expect_smaller(sum(as.matrix(Z0)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
  expect_smaller(sum(as.matrix(Z1)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
  expect_smaller(sum(as.matrix(Z2)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + inverse",
  {
    list_covariance = list("resp1" = "inverse")
    inv_Sigma <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Sigma <- solve(inv_Sigma)
    D_Z0 <- -Sigma%*%Z0%*%Sigma
    D_Z1 <- -Sigma%*%Z1%*%Sigma
    D_Z2 <- -Sigma%*%Z2%*%Sigma
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Z0)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Z1)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Z2)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + expm",
  {

    list_covariance = list("resp1" = "expm")
    U <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    D_Sigma1 <- mc_dexp_gold(M = U, dM = Z0)
    D_Sigma2 <- mc_dexp_gold(M = U, dM = Z1)
    D_Sigma3 <- mc_dexp_gold(M = U, dM = Z2)
    Sigma <- D_Sigma1[[1]]
    inv_Sigma <- solve(Sigma)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma1[[2]])-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma2[[2]])-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma3[[2]])-as.matrix(actual$D_C[[3]])), tol = 1e-05)
  }
)

##################################################################################
### tweedie and binomialP variance functions #####################################
##################################################################################
name.variance <- c("tweedie","binomialP")
for(i in 1:2){
  list_variance <- list("resp1" = name.variance[i])
  print(paste("Testing variance function:", name.variance[i]))
  if(name.variance[i] == "tweedie"){variance = "power"}
  if(name.variance[i] == "binomialP"){variance = "binomialP"}
test_that(
  "Checking derivatives - Univariate case + identity",
  {

    Omega <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    V <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = rep(1,10), variance = variance,
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_p%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_p
    D_Sigma_Z0 <- V$V_sqrt%*%Z0%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%Z1%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%Z2%*%V$V_sqrt
    list_covariance = list("resp1" = "identity")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(1),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z0)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z1)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z2)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + inverse",
  {
    inv_Omega <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega <- solve(inv_Omega)
    inv_V <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = 1, variance = variance,
                              inverse = TRUE, derivative_power = TRUE, derivative_mu = FALSE)
    V <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = 1, variance = variance,
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    inv_Sigma <- inv_V$V_inv_sqrt%*%inv_Omega%*%inv_V$V_inv_sqrt
    Sigma <- solve(inv_Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%(-Omega%*%Z0%*%Omega)%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(-Omega%*%Z1%*%Omega)%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(-Omega%*%Z2%*%Omega)%*%V$V_sqrt
    list_covariance = list("resp1" = "inverse")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z0)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z1)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z2)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + expm",
  {
    list_tau = list("resp1" = c(1,0.3,0.1))
    list_covariance = list("resp1" = "expm")
    V <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = 1, variance = variance,
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    U <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z0)
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z1)
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z2)
    D_Sigma_power <- V$D_V_sqrt_power%*%D_Omega1[[1]]%*%V$V_sqrt + V$V_sqrt%*%D_Omega1[[1]]%*%V$D_V_sqrt
    D_Sigma1 <- V$V_sqrt%*%D_Omega1[[2]]%*%V$V_sqrt
    D_Sigma2 <- V$V_sqrt%*%D_Omega2[[2]]%*%V$V_sqrt
    D_Sigma3 <- V$V_sqrt%*%D_Omega3[[2]]%*%V$V_sqrt
    Sigma <- V$V_sqrt%*%D_Omega1[[1]]%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma1)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma2)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma3)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
  }
)
print("OK")
}
print("Checking Poisson-Tweedie derivatives")
##########################################################################################
## poisson_tweedie variance function #####################################################
##########################################################################################
test_that(
  "Checking derivatives - Univariate case + identity",
  {

    Omega <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    V <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = rep(1,10), variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- Diagonal(length(mu1$mu), mu1$mu) + V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_p%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_p
    D_Sigma_Z0 <- V$V_sqrt%*%Z0%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%Z1%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%Z2%*%V$V_sqrt
    list_covariance = list("resp1" = "identity")
    list_variance = list("resp1" = "poisson_tweedie")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(1),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z0)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z1)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z2)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + inverse",
  {
    inv_Omega <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega <- solve(inv_Omega)
    V <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = 1, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- Diagonal(length(mu1$mu), mu1$mu) + V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power <- V$D_V_sqrt_power%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt
    D_Sigma_Z0 <- V$V_sqrt%*%(-Omega%*%Z0%*%Omega)%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(-Omega%*%Z1%*%Omega)%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(-Omega%*%Z2%*%Omega)%*%V$V_sqrt
    list_covariance = list("resp1" = "inverse")
    list_variance = list("resp1" = "poisson_tweedie")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z0)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z1)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z2)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + expm",
  {
    V <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = 1, variance = "power",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    U <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z0)
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z1)
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z2)
    D_Sigma_power <- V$D_V_sqrt_power%*%D_Omega1[[1]]%*%V$V_sqrt + V$V_sqrt%*%D_Omega1[[1]]%*%V$D_V_sqrt
    D_Sigma1 <- V$V_sqrt%*%D_Omega1[[2]]%*%V$V_sqrt
    D_Sigma2 <- V$V_sqrt%*%D_Omega2[[2]]%*%V$V_sqrt
    D_Sigma3 <- V$V_sqrt%*%D_Omega3[[2]]%*%V$V_sqrt
    Sigma <- Diagonal(length(mu1$mu), mu1$mu) + V$V_sqrt%*%D_Omega1[[1]]%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    list_covariance = list("resp1" = "expm")
    list_variance = list("resp1" = "poisson_tweedie")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma1)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma2)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma3)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
  }
)

print("Checking binomiaPQ derivatives")
##########################################################################################
## binomialPQ variance function ##########################################################
##########################################################################################
test_that(
  "Checking derivatives - Univariate case + identity",
  {

    Omega <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    V <- mc_variance_function(mu = mu1$mu, power = c(2,2), Ntrial = rep(1,10),
                              variance = "binomialPQ",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power1 <- V$D_V_sqrt_p%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_p
    D_Sigma_power2 <- V$D_V_sqrt_q%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_q
    D_Sigma_Z0 <- V$V_sqrt%*%Z0%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%Z1%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%Z2%*%V$V_sqrt
    list_covariance = list("resp1" = "identity")
    list_variance = list("resp1" = "binomialPQ")
    list_power = list("resp1" = c(2,2))
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(1),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power1)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_power2)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z0)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z1)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z2)-as.matrix(actual$D_C[[5]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + inverse",
  {
    inv_Omega <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega <- solve(inv_Omega)
    V <- mc_variance_function(mu = mu1$mu, power = c(2,2), Ntrial = 1, variance = "binomialPQ",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    Sigma <- V$V_sqrt%*%Omega%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    D_Sigma_power1 <- V$D_V_sqrt_p%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_p
    D_Sigma_power2 <- V$D_V_sqrt_q%*%Omega%*%V$V_sqrt + V$V_sqrt%*%Omega%*%V$D_V_sqrt_q
    D_Sigma_Z0 <- V$V_sqrt%*%(-Omega%*%Z0%*%Omega)%*%V$V_sqrt
    D_Sigma_Z1 <- V$V_sqrt%*%(-Omega%*%Z1%*%Omega)%*%V$V_sqrt
    D_Sigma_Z2 <- V$V_sqrt%*%(-Omega%*%Z2%*%Omega)%*%V$V_sqrt
    list_covariance = list("resp1" = "inverse")
    list_variance = list("resp1" = "binomialPQ")
    list_power = list("resp1" = c(2,2))
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power1)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_power2)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z0)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z1)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_Z2)-as.matrix(actual$D_C[[5]])), tol = 1e-05)
  }
)

test_that(
  "Checking derivatives - Univariate case + expm",
  {
    V <- mc_variance_function(mu = mu1$mu, power = c(2,2), Ntrial = 1, variance = "binomialPQ",
                              inverse = FALSE, derivative_power = TRUE, derivative_mu = FALSE)
    U <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    D_Omega1 <- mc_dexp_gold(M = U, dM = Z0)
    D_Omega2 <- mc_dexp_gold(M = U, dM = Z1)
    D_Omega3 <- mc_dexp_gold(M = U, dM = Z2)
    D_Sigma_power1 <- V$D_V_sqrt_p%*%D_Omega1[[1]]%*%V$V_sqrt +
      V$V_sqrt%*%D_Omega1[[1]]%*%V$D_V_sqrt_p
    D_Sigma_power2 <- V$D_V_sqrt_q%*%D_Omega1[[1]]%*%V$V_sqrt +
      V$V_sqrt%*%D_Omega1[[1]]%*%V$D_V_sqrt_q
    D_Sigma1 <- V$V_sqrt%*%D_Omega1[[2]]%*%V$V_sqrt
    D_Sigma2 <- V$V_sqrt%*%D_Omega2[[2]]%*%V$V_sqrt
    D_Sigma3 <- V$V_sqrt%*%D_Omega3[[2]]%*%V$V_sqrt
    Sigma <- V$V_sqrt%*%D_Omega1[[1]]%*%V$V_sqrt
    inv_Sigma <- solve(Sigma)
    list_covariance = list("resp1" = "expm")
    list_variance = list("resp1" = "binomialPQ")
    list_power = list("resp1" = c(2,2))
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(inv_Sigma), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_smaller(sum(as.matrix(D_Sigma_power1)-as.matrix(actual$D_C[[1]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma_power2)-as.matrix(actual$D_C[[2]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma1)-as.matrix(actual$D_C[[3]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma2)-as.matrix(actual$D_C[[4]])), tol = 1e-05)
    expect_smaller(sum(as.matrix(D_Sigma3)-as.matrix(actual$D_C[[5]])), tol = 1e-05)
  }
)

##########################################################################
## Bivariate case - Checking the calculations ############################
##########################################################################
list_mu <- list("resp1" = mu1, "resp2" = mu2)
list_Ntrial <- list("resp1" = rep(1,10), "resp2" = rep(1,10))
list_tau <- list("resp1" = c(1,0.1), "resp2" = c(2,0.8,0.3))
list_power <- list("resp1" = c(2), "resp2" = c(1))
list_Z <- list("resp1" = list(Z0,Z1), "resp2" = list(Z0,Z1,Z2))
list_sparse = list("resp1" = FALSE, "resp2" = FALSE)
list_variance = list("resp1" ="constant", "resp2" = "constant")
list_covariance = list("resp1" = "identity", "resp2" = "identity")
list_power_fixed = list("resp1" = FALSE, "resp2" = FALSE)

## Identity covariance function
test_that(
  "Checking inverse and derivatives - Bivariate case + identity",
  {
    Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    BLOCO_ZERO <- Matrix(0,10,10)
    Sigma <- bdiag(Omega1,Omega2)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                    rho = c(0),
                    list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                    list_sparse = list_sparse, list_variance = list_variance,
                    list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(solve(Sigma), actual$inv_C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(bdiag(Z0,BLOCO_ZERO)), as.matrix(actual$D_C[[2]]))
    expect_equal(as.matrix(bdiag(Z1,BLOCO_ZERO)), as.matrix(actual$D_C[[3]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,Z0)), as.matrix(actual$D_C[[4]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,Z1)), as.matrix(actual$D_C[[5]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,Z2)), as.matrix(actual$D_C[[6]]))
  }
)

## Inverse covariance function
test_that(
  "Checking inverse and derivatives - Bivariate case + inverse",
  {
    inv_Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    inv_Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    BLOCO_ZERO <- Matrix(0,10,10)
    inv_Sigma <- bdiag(inv_Omega1,inv_Omega2)
    Sigma <- solve(inv_Sigma)
    D_Z01 <- -Sigma%*%bdiag(Z0,BLOCO_ZERO)%*%Sigma
    D_Z11 <- -Sigma%*%bdiag(Z1,BLOCO_ZERO)%*%Sigma
    D_Z02 <- -Sigma%*%bdiag(BLOCO_ZERO,Z0)%*%Sigma
    D_Z12 <- -Sigma%*%bdiag(BLOCO_ZERO,Z1)%*%Sigma
    D_Z22 <- -Sigma%*%bdiag(BLOCO_ZERO,Z2)%*%Sigma
    list_covariance <- list("resp1" = "inverse", "resp2" = "inverse")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(D_Z01), as.matrix(actual$D_C[[2]]))
    expect_equal(as.matrix(D_Z11), as.matrix(actual$D_C[[3]]))
    expect_equal(as.matrix(D_Z02), as.matrix(actual$D_C[[4]]))
    expect_equal(as.matrix(D_Z12), as.matrix(actual$D_C[[5]]))
    expect_equal(as.matrix(D_Z22), as.matrix(actual$D_C[[6]]))
  }
)

## Exponential-matrix covariance function
test_that(
  "Checking inverse and derivatives - Bivariate case + expm",
  {
    BLOCO_ZERO <- Matrix(0,10,10)
    U1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    U2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    U <- bdiag(U1,U2)
    D_Sigma01 <- mc_dexp_gold(M = U, dM = bdiag(Z0,BLOCO_ZERO))
    D_Sigma11 <- mc_dexp_gold(M = U, dM = bdiag(Z1,BLOCO_ZERO))
    D_Sigma02 <- mc_dexp_gold(M = U, dM = bdiag(BLOCO_ZERO,Z0))
    D_Sigma12 <- mc_dexp_gold(M = U, dM = bdiag(BLOCO_ZERO,Z1))
    D_Sigma22 <- mc_dexp_gold(M = U, dM = bdiag(BLOCO_ZERO,Z2))
    Sigma <- D_Sigma01[[1]]
    inv_Sigma <- solve(Sigma)
    list_covariance <- list("resp1" = "expm", "resp2" = "expm")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(D_Sigma01[[2]]), as.matrix(actual$D_C[[2]]))
    expect_equal(as.matrix(D_Sigma11[[2]]), as.matrix(actual$D_C[[3]]))
    expect_equal(as.matrix(D_Sigma02[[2]]), as.matrix(actual$D_C[[4]]))
    expect_equal(as.matrix(D_Sigma12[[2]]), as.matrix(actual$D_C[[5]]))
    expect_equal(as.matrix(D_Sigma22[[2]]), as.matrix(actual$D_C[[6]]))
  }
)

###### Mixed case - Identity + inverse
test_that(
  "Checking inverse and derivatives - Bivariate case + identity + inverse",
  {
    Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega2 <- solve(mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]]))
    BLOCO_ZERO <- Matrix(0,10,10)
    Sigma <- bdiag(Omega1,Omega2)
    list_covariance = list("resp1" = "identity", "resp2" = "inverse")
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed)
    expect_equal(solve(Sigma), actual$inv_C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(bdiag(Z0,BLOCO_ZERO)), as.matrix(actual$D_C[[2]]))
    expect_equal(as.matrix(bdiag(Z1,BLOCO_ZERO)), as.matrix(actual$D_C[[3]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,Z0)%*%Sigma), as.matrix(actual$D_C[[4]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,Z1)%*%Sigma), as.matrix(actual$D_C[[5]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,Z2)%*%Sigma), as.matrix(actual$D_C[[6]]))
  }
)

##########################################################################
## Bivariate case - Checking derivatives with respect to beta ############
##########################################################################
x1 <- seq(-1,1, l=10)
X <- model.matrix(~x1)
mu1 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "logit")
mu2 <- mc_link_function(beta = c(1,0.4), X = X, offset = NULL, link = "logit")
Z0 <- Diagonal(10, 1)
Z1 <- Matrix(rep(1,10)%*%t(rep(1,10)))
Z2 <- Matrix(c(rep(0,5),rep(1,5))%*%t(c(rep(0,5),rep(1,5))))

list_mu <- list("resp1" = mu1, "resp2" = mu2)
list_Ntrial <- list("resp1" = rep(1,10), "resp2" = rep(1,10))
list_tau <- list("resp1" = c(1,0.1), "resp2" = c(2,0.8,0.3))
list_power <- list("resp1" = c(2), "resp2" = c(1))
list_Z <- list("resp1" = list(Z0,Z1), "resp2" = list(Z0,Z1,Z2))
list_sparse = list("resp1" = FALSE, "resp2" = FALSE)
list_variance = list("resp1" ="tweedie", "resp2" = "tweedie")
list_covariance = list("resp1" = "identity", "resp2" = "identity")
list_power_fixed = list("resp1" = FALSE, "resp2" = FALSE)
compute_derivative_beta <- TRUE

## Variance function: tweedie | Covariance function: identity
test_that(
  "Checking derivatives with respect to beta - tweedie + identity",
  {
    Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    V_sqrt1 <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = NULL, variance = "power",
                                   inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    V_sqrt2 <- mc_variance_function(mu = mu2$mu, power = 1, Ntrial = NULL, variance = "power",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    Sigma1 <- V_sqrt1$V_sqrt%*%Omega1%*%V_sqrt1$V_sqrt
    Sigma2 <- V_sqrt2$V_sqrt%*%Omega2%*%V_sqrt2$V_sqrt
    Sigma <- bdiag(Sigma1,Sigma2)
    D_Sigma1_beta0 <- Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma1_beta1 <- Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma2_beta0 <- Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)
    D_Sigma2_beta1 <- Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)
    list_covariance = list("resp1" = "identity", "resp2" = "identity")
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta0,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta1,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta0)), as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta1)), as.matrix(actual$D_C_beta[[4]]))
    }
)

## Variance function: binomialP | Covariance function: identity
test_that(
  "Checking derivatives with respect to beta - binomialP + identity",
  {
    Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    V_sqrt1 <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = rep(1,10),
                                    variance = "binomialP",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    V_sqrt2 <- mc_variance_function(mu = mu2$mu, power = 1, Ntrial = rep(1,10),
                                    variance = "binomialP",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    Sigma1 <- V_sqrt1$V_sqrt%*%Omega1%*%V_sqrt1$V_sqrt
    Sigma2 <- V_sqrt2$V_sqrt%*%Omega2%*%V_sqrt2$V_sqrt
    Sigma <- bdiag(Sigma1,Sigma2)
    D_Sigma1_beta0 <- Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma1_beta1 <- Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma2_beta0 <- Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)
    D_Sigma2_beta1 <- Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)
    list_covariance = list("resp1" = "identity", "resp2" = "identity")
    list_variance = list("resp1" =  "binomialP", "resp2" = "binomialP")
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta0,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta1,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta0)), as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta1)), as.matrix(actual$D_C_beta[[4]]))
  }
)

## Variance function: binomialPQ | Covariance function: identity
test_that(
  "Checking derivatives with respect to beta - binomialPQ + identity",
  {
    Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    V_sqrt1 <- mc_variance_function(mu = mu1$mu, power = c(2,1), Ntrial = rep(1,10),
                                    variance = "binomialPQ",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    V_sqrt2 <- mc_variance_function(mu = mu2$mu, power = c(1,2), Ntrial = rep(1,10),
                                    variance = "binomialPQ",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    Sigma1 <- V_sqrt1$V_sqrt%*%Omega1%*%V_sqrt1$V_sqrt
    Sigma2 <- V_sqrt2$V_sqrt%*%Omega2%*%V_sqrt2$V_sqrt
    Sigma <- bdiag(Sigma1,Sigma2)
    D_Sigma1_beta0 <- Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma1_beta1 <- Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma2_beta0 <- Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)
    D_Sigma2_beta1 <- Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)
    list_covariance = list("resp1" = "identity", "resp2" = "identity")
    list_variance = list("resp1" =  "binomialPQ", "resp2" = "binomialPQ")
    list_power = list("resp1" = c(2,1), "resp2" = c(1,2))
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta0,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta1,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta0)), as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta1)), as.matrix(actual$D_C_beta[[4]]))
  }
)

## Variance function: poisson_tweedie | Covariance function: identity
test_that(
  "Checking derivatives with respect to beta - poisson_tweedie + identity",
  {
    Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    V_sqrt1 <- mc_variance_function(mu = mu1$mu, power = c(2), Ntrial = rep(1,10),
                                    variance = "power",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    V_sqrt2 <- mc_variance_function(mu = mu2$mu, power = c(1), Ntrial = rep(1,10),
                                    variance = "power",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    Sigma1 <- Diagonal(10, mu1$mu) + V_sqrt1$V_sqrt%*%Omega1%*%V_sqrt1$V_sqrt
    Sigma2 <- Diagonal(10, mu2$mu) + V_sqrt2$V_sqrt%*%Omega2%*%V_sqrt2$V_sqrt
    Sigma <- bdiag(Sigma1,Sigma2)
    D_Sigma1_beta0 <- Diagonal(10, mu1$D[,1]) +
      Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma1_beta1 <- Diagonal(10, mu1$D[,2]) +
      Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma2_beta0 <- Diagonal(10, mu2$D[,1]) +
      Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)
    D_Sigma2_beta1 <- Diagonal(10, mu2$D[,2]) +
      Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)
    list_covariance = list("resp1" = "identity", "resp2" = "identity")
    list_variance = list("resp1" =  "poisson_tweedie", "resp2" = "poisson_tweedie")
    list_power = list("resp1" = c(2), "resp2" = c(1))
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta0,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta1,BLOCO_ZERO)), as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta0)), as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta1)), as.matrix(actual$D_C_beta[[4]]))
  }
)

############
## Variance function: tweedie | Covariance function: inverse
test_that(
  "Checking derivatives with respect to beta - tweedie + inverse",
  {
    inv_Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    inv_Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    V_inv_sqrt1 <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = NULL, variance = "power",
                                    inverse = TRUE, derivative_power = TRUE, derivative_mu = TRUE)
    V_inv_sqrt2 <- mc_variance_function(mu = mu2$mu, power = 1, Ntrial = NULL, variance = "power",
                                    inverse = TRUE, derivative_power = TRUE, derivative_mu = TRUE)
    inv_Sigma1 <- V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt
    inv_Sigma2 <- V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt
    inv_Sigma <- bdiag(inv_Sigma1,inv_Sigma2)
    Sigma <- solve(inv_Sigma)
    D_inv_Sigma1_beta0 <- Diagonal(10,mu1$D[,1]*V_inv_sqrt1$D_V_inv_sqrt_mu)%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt +
      V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%Diagonal(10,mu1$D[,1]*V_inv_sqrt1$D_V_inv_sqrt_mu)
    D_inv_Sigma1_beta1 <- Diagonal(10,mu1$D[,2]*V_inv_sqrt1$D_V_inv_sqrt_mu)%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt +
      V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%Diagonal(10,mu1$D[,2]*V_inv_sqrt1$D_V_inv_sqrt_mu)
    D_inv_Sigma2_beta0 <- Diagonal(10,mu2$D[,1]*V_inv_sqrt2$D_V_inv_sqrt_mu)%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt +
      V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%Diagonal(10,mu2$D[,1]*V_inv_sqrt2$D_V_inv_sqrt_mu)
    D_inv_Sigma2_beta1 <- Diagonal(10,mu2$D[,2]*V_inv_sqrt2$D_V_inv_sqrt_mu)%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt +
      V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%Diagonal(10,mu2$D[,2]*V_inv_sqrt2$D_V_inv_sqrt_mu)
    list_variance = list("resp1" = "tweedie", "resp2" = "tweedie")
    list_covariance = list("resp1" = "inverse", "resp2" = "inverse")
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(-Sigma%*%bdiag(D_inv_Sigma1_beta0,BLOCO_ZERO)%*%Sigma),
                 as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(D_inv_Sigma1_beta1,BLOCO_ZERO)%*%Sigma),
                 as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,D_inv_Sigma2_beta0)%*%Sigma),
                 as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,D_inv_Sigma2_beta1)%*%Sigma),
                 as.matrix(actual$D_C_beta[[4]]))
  }
)

## Variance function: binomialP | Covariance function: inverse
test_that(
  "Checking derivatives with respect to beta - binomialP + inverse",
  {
    inv_Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    inv_Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    V_inv_sqrt1 <- mc_variance_function(mu = mu1$mu, power = 2, Ntrial = rep(1,10),
                                        variance = "binomialP",
                                        inverse = TRUE, derivative_power = TRUE, derivative_mu = TRUE)
    V_inv_sqrt2 <- mc_variance_function(mu = mu2$mu, power = 1, Ntrial = rep(1,10),
                                        variance = "binomialP",
                                        inverse = TRUE, derivative_power = TRUE, derivative_mu = TRUE)
    inv_Sigma1 <- V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt
    inv_Sigma2 <- V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt
    inv_Sigma <- bdiag(inv_Sigma1,inv_Sigma2)
    Sigma <- solve(inv_Sigma)
    D_inv_Sigma1_beta0 <- Diagonal(10,mu1$D[,1]*V_inv_sqrt1$D_V_inv_sqrt_mu)%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt +
      V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%Diagonal(10,mu1$D[,1]*V_inv_sqrt1$D_V_inv_sqrt_mu)
    D_inv_Sigma1_beta1 <- Diagonal(10,mu1$D[,2]*V_inv_sqrt1$D_V_inv_sqrt_mu)%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt +
      V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%Diagonal(10,mu1$D[,2]*V_inv_sqrt1$D_V_inv_sqrt_mu)
    D_inv_Sigma2_beta0 <- Diagonal(10,mu2$D[,1]*V_inv_sqrt2$D_V_inv_sqrt_mu)%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt +
      V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%Diagonal(10,mu2$D[,1]*V_inv_sqrt2$D_V_inv_sqrt_mu)
    D_inv_Sigma2_beta1 <- Diagonal(10,mu2$D[,2]*V_inv_sqrt2$D_V_inv_sqrt_mu)%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt +
      V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%Diagonal(10,mu2$D[,2]*V_inv_sqrt2$D_V_inv_sqrt_mu)
    list_variance = list("resp1" = "binomialP", "resp2" = "binomialP")
    list_covariance = list("resp1" = "inverse", "resp2" = "inverse")
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(-Sigma%*%bdiag(D_inv_Sigma1_beta0,BLOCO_ZERO)%*%Sigma),
                 as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(D_inv_Sigma1_beta1,BLOCO_ZERO)%*%Sigma),
                 as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,D_inv_Sigma2_beta0)%*%Sigma),
                 as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,D_inv_Sigma2_beta1)%*%Sigma),
                 as.matrix(actual$D_C_beta[[4]]))
  }
)

## Variance function: binomialPQ | Covariance function: inverse
test_that(
  "Checking derivatives with respect to beta - binomialPQ + inverse",
  {
    inv_Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    inv_Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    V_inv_sqrt1 <- mc_variance_function(mu = mu1$mu, power = c(2,1), Ntrial = rep(1,10),
                                        variance = "binomialPQ",
                                        inverse = TRUE, derivative_power = TRUE, derivative_mu = TRUE)
    V_inv_sqrt2 <- mc_variance_function(mu = mu2$mu, power = c(1,2), Ntrial = rep(1,10),
                                        variance = "binomialPQ",
                                        inverse = TRUE, derivative_power = TRUE, derivative_mu = TRUE)
    inv_Sigma1 <- V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt
    inv_Sigma2 <- V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt
    inv_Sigma <- bdiag(inv_Sigma1,inv_Sigma2)
    Sigma <- solve(inv_Sigma)
    D_inv_Sigma1_beta0 <- Diagonal(10,mu1$D[,1]*V_inv_sqrt1$D_V_inv_sqrt_mu)%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt +
      V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%Diagonal(10,mu1$D[,1]*V_inv_sqrt1$D_V_inv_sqrt_mu)
    D_inv_Sigma1_beta1 <- Diagonal(10,mu1$D[,2]*V_inv_sqrt1$D_V_inv_sqrt_mu)%*%inv_Omega1%*%V_inv_sqrt1$V_inv_sqrt +
      V_inv_sqrt1$V_inv_sqrt%*%inv_Omega1%*%Diagonal(10,mu1$D[,2]*V_inv_sqrt1$D_V_inv_sqrt_mu)
    D_inv_Sigma2_beta0 <- Diagonal(10,mu2$D[,1]*V_inv_sqrt2$D_V_inv_sqrt_mu)%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt +
      V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%Diagonal(10,mu2$D[,1]*V_inv_sqrt2$D_V_inv_sqrt_mu)
    D_inv_Sigma2_beta1 <- Diagonal(10,mu2$D[,2]*V_inv_sqrt2$D_V_inv_sqrt_mu)%*%inv_Omega2%*%V_inv_sqrt2$V_inv_sqrt +
      V_inv_sqrt2$V_inv_sqrt%*%inv_Omega2%*%Diagonal(10,mu2$D[,2]*V_inv_sqrt2$D_V_inv_sqrt_mu)
    list_variance = list("resp1" = "binomialPQ", "resp2" = "binomialPQ")
    list_covariance = list("resp1" = "inverse", "resp2" = "inverse")
    list_power = list("resp1" = c(2,1), "resp2" = c(1,2))
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(-Sigma%*%bdiag(D_inv_Sigma1_beta0,BLOCO_ZERO)%*%Sigma),
                 as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(D_inv_Sigma1_beta1,BLOCO_ZERO)%*%Sigma),
                 as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,D_inv_Sigma2_beta0)%*%Sigma),
                 as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(-Sigma%*%bdiag(BLOCO_ZERO,D_inv_Sigma2_beta1)%*%Sigma),
                 as.matrix(actual$D_C_beta[[4]]))
  }
)

## Variance function: poisson_tweedie | Covariance function: inverse
test_that(
  "Checking derivatives with respect to beta - poisson_tweedie + inverse",
  {
    inv_Omega1 <- mc_matrix_linear_predictor(tau = list_tau[[1]], Z = list_Z[[1]])
    inv_Omega2 <- mc_matrix_linear_predictor(tau = list_tau[[2]], Z = list_Z[[2]])
    Omega1 <- solve(inv_Omega1)
    Omega2 <- solve(inv_Omega2)
    V_sqrt1 <- mc_variance_function(mu = mu1$mu, power = c(2), Ntrial = rep(1,10),
                                    variance = "power",
                                    inverse = FALSE, derivative_power = TRUE,
                                    derivative_mu = TRUE)
    V_sqrt2 <- mc_variance_function(mu = mu2$mu, power = c(1), Ntrial = rep(1,10),
                                    variance = "power",
                                    inverse = FALSE, derivative_power = TRUE, derivative_mu = TRUE)
    Sigma1 <- V_sqrt1$V_sqrt%*%Omega1%*%V_sqrt1$V_sqrt
    Sigma2 <- V_sqrt2$V_sqrt%*%Omega2%*%V_sqrt2$V_sqrt
    Sigma <- bdiag(Sigma1,Sigma2)
    Sigma <- Diagonal(20, c(mu1$mu, mu2$mu)) + Sigma
    inv_Sigma <- solve(Sigma)
    D_Sigma1_beta0 <- Diagonal(10, mu1$D[,1]) +
      Diagonal(10, mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10, mu1$D[,1]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma1_beta1 <- Diagonal(10, mu1$D[,2]) +
      Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)%*%Omega1%*%V_sqrt1$V_sqrt +
      V_sqrt1$V_sqrt%*%Omega1%*%Diagonal(10,mu1$D[,2]*V_sqrt1$D_V_sqrt_mu)
    D_Sigma2_beta0 <- Diagonal(10, mu2$D[,1]) +
      Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,1]*V_sqrt2$D_V_sqrt_mu)
    D_Sigma2_beta1 <- Diagonal(10, mu2$D[,2]) +
      Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)%*%Omega2%*%V_sqrt2$V_sqrt +
      V_sqrt2$V_sqrt%*%Omega2%*%Diagonal(10,mu2$D[,2]*V_sqrt2$D_V_sqrt_mu)
    list_variance = list("resp1" = "poisson_tweedie", "resp2" = "poisson_tweedie")
    list_covariance = list("resp1" = "inverse", "resp2" = "inverse")
    list_power = list("resp1" = c(2), "resp2" = c(1))
    BLOCO_ZERO <- Matrix(0,10,10)
    actual = mc_build_C(list_mu = list_mu, list_Ntrial = list_Ntrial,
                        rho = c(0),
                        list_tau = list_tau, list_Z = list_Z, list_power = list_power,
                        list_sparse = list_sparse, list_variance = list_variance,
                        list_covariance = list_covariance, list_power_fixed = list_power_fixed,
                        compute_C = TRUE, compute_derivative_beta = TRUE)
    expect_equal(as.matrix(solve(Sigma)), as.matrix(actual$inv_C))
    expect_equal(Sigma, actual$C)
    expect_equal(as.matrix(Sigma), as.matrix(solve(actual$inv_C)))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta0,BLOCO_ZERO)),
                 as.matrix(actual$D_C_beta[[1]]))
    expect_equal(as.matrix(bdiag(D_Sigma1_beta1,BLOCO_ZERO)),
                 as.matrix(actual$D_C_beta[[2]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta0)),
                 as.matrix(actual$D_C_beta[[3]]))
    expect_equal(as.matrix(bdiag(BLOCO_ZERO,D_Sigma2_beta1)),
                 as.matrix(actual$D_C_beta[[4]]))
  }
)

print("test_mc_build.C - OK")
