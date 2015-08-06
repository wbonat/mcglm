## Testing quasi-score function
library(mcglm)
x <- seq(-1,1, l = 10)
X <- model.matrix(~ x)
mu1 <- mc_link_function(beta = c(1,0.5), X = X, offset = NULL, link = "log")
mu2 <- mc_link_function(beta = c(1,-0.5), X = X, offset = NULL, link = "logit")
mu_vec <- c(mu1$mu, mu2$mu)
D <- bdiag(mu1$D, mu2$D)
y_vec <- rpois(20, lambda = mu_vec)
Z0 <- Diagonal(10, 1)
Z1 <- Matrix(rep(1,10)%*%t(rep(1,10)))

Cfeatures <- mc_build_C(list_mu = list("resp1" = mu1, "resp2" = mu2),
                        list_Ntrial = list("NULL", rep(1,10)),
                        rho = 0.8, list_tau = list(c(1,0.5), c(1,0.2)),
                        list_power = list("resp1" = 2, "resp2" = c(2,1)),
                        list_Z = list("resp1" =  list(Z0,Z1), "resp2" = list(Z0,Z1)),
                        list_sparse = list(FALSE, FALSE),
                        list_variance = list("tweedie", "binomialPQ"),
                        list_covariance = list("identity", "identity"),
                        list_power_fixed = list(FALSE,FALSE), compute_C = FALSE,
                        compute_derivative_beta = TRUE)

tt = mc_quasi_score(D = D, inv_C = Cfeatures$inv_C, y_vec = y_vec, mu_vec = mu_vec)
tt$Sensitivity
inv_J_beta <- solve(tt$Sensitivity)

tt1 = mc_pearson(y_vec = y_vec, mu_vec = mu_vec, Cfeatures = Cfeatures, inv_J_beta = inv_J_beta, D = D,
           correct = TRUE, compute_variability = TRUE)
names(tt1)
