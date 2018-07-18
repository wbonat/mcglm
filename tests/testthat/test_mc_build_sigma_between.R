print("test_mc_build_sigma_between.R")
## Tests mc_build_sigmab

test_that(
  "length of output - Sigmab",
  {
    expect1 <- 5
    n_derivatives <- 5*(5-1)/2
    Sigmab <- mc_build_sigma_between(n_resp = 5, inverse = FALSE,
                           rho = c(0.12, 0.13, 0.14, 0.15, 0.23, 0.24, 0.25, 0.34, 0.35, 0.45))
    expect_equal(expect1, dim(Sigmab$Sigmab)[1])
    expect_equal(expect1, dim(Sigmab$Sigmab)[2])
    expect_equal(n_derivatives, length(Sigmab$D_Sigmab))
    }
)

test_that(
  "length of output - inverse Sigmab",
  {
    expect1 <- 5
    n_derivatives <- 5*(5-1)/2
    inv_Sigmab <- mc_build_sigma_between(n_resp = 5, inverse = TRUE,
                          rho = c(0.12, 0.13, 0.14, 0.15, 0.23, 0.24, 0.25, 0.34, 0.35, 0.45))
    expect_equal(expect1, dim(inv_Sigmab$inv_Sigmab)[1])
    expect_equal(expect1, dim(inv_Sigmab$inv_Sigmab)[2])
    expect_equal(n_derivatives, length(inv_Sigmab$D_inv_Sigmab))
  }
)

test_that(
  "Checking the derivatives of inverse of Sigmab",
  {
    Sigmab <- mc_build_sigma_between(rho = c(0.12,0.13,0.23), inverse = FALSE, n_resp = 3)
    inv_Sigmab_exp <- solve(Sigmab$Sigmab)
    D_inv_Sigmab1 <- -inv_Sigmab_exp%*%Sigmab$D_Sigmab[[1]]%*%inv_Sigmab_exp
    D_inv_Sigmab2 <- -inv_Sigmab_exp%*%Sigmab$D_Sigmab[[2]]%*%inv_Sigmab_exp
    D_inv_Sigmab3 <- -inv_Sigmab_exp%*%Sigmab$D_Sigmab[[3]]%*%inv_Sigmab_exp
    inv_Sigmab <- mc_build_sigma_between(n_resp = 3, inverse = TRUE,
                                         rho = c(0.12, 0.13, 0.23))
    expect_equal(inv_Sigmab_exp, inv_Sigmab$inv_Sigmab)
    expect_equal(D_inv_Sigmab1,  inv_Sigmab$D_inv_Sigmab[[1]])
    expect_equal(D_inv_Sigmab2,  inv_Sigmab$D_inv_Sigmab[[2]])
    expect_equal(D_inv_Sigmab3,  inv_Sigmab$D_inv_Sigmab[[3]])
  }
)
print("test_mc_build_sigma_between.R-OK")
