# Set of examples 1 - Univariate models --------------------------------
# Author: Wagner Hugo Bonat LEG/IMADA ----------------------------------
# Date: 07/08/2015 -----------------------------------------------------
# Lastest updated: 28/08/2015 ------------------------------------------
#-----------------------------------------------------------------------
rm(list=ls())

# Loading extra package ------------------------------------------------

devtools::load_all("../")
## require(mcglm)
require(Matrix)
require(tweedie)
require(dplyr)
require(mvtnorm)

# Setting the seed -----------------------------------------------------
set.seed(2503)

# Case 1 - Linear regression model -------------------------------------
covariate <- seq(-1,1, l = 100)
X <- model.matrix(~ covariate)
mu1 <- mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "identity")
y1 <- rnorm(100, mu1$mu, sd = 0.5)
Z0 <- Diagonal(100, 1)
data <- data.frame("y1" = y1, "covariate" = covariate)

# Linear Regression model ----------------------------------------------
fit1.id <- mcglm(linear_pred = c(y1 ~ covariate), 
                 matrix_pred = list("resp1" = list(Z0)),
                 data = data, 
                 control_algorithm = list("correct" = FALSE, 
                                          "verbose" = FALSE))
summary(fit1.id)

methods(class = "mcglm")

fit1.id
anova(fit1.id)
coef(fit1.id)
confint(fit1.id)
fitted(fit1.id)
plot(fit1.id) ## residuals
plot(fit1.id, type = "algorithm")
plot(fit1.id, type = "partial")
residuals(fit1.id)
summary(fit1.id)
vcov(fit1.id)

# Using the inverse covariance link function ---------------------------
fit1.inv <- mcglm(linear_pred = c(y1 ~ covariate), 
                  matrix_pred = list("resp1" = list(Z0)),
                  covariance = "inverse", data = data,
                  control_algorithm = list("verbose" = FALSE, 
                                           "correct" = FALSE))
summary(fit1.inv)

# Using the exponential-matrix covariance link function ----------------
fit1.expm <- mcglm(linear_pred = c(y1 ~ covariate), 
                   matrix_pred = list("resp1" = list(Z0)),
                   covariance = "expm", data = data,
                   control_algorithm = list("verbose" = FALSE, 
                                            "correct" = FALSE))
summary(fit1.expm)

# Comparing estimates of tau using diferent covariance link functions --
coef(fit1.id, type = "tau")$Estimates
1/coef(fit1.inv, type = "tau")$Estimates
exp(coef(fit1.expm, type = "tau")$Estimates)

# Case 2 - Linear regression model with heteroscedasticity -------------
covariate <- seq(-1,1, l = 100)
X <- model.matrix(~ covariate)
mu1 <- mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "identity")
Z0 <- Diagonal(100, 1)
Z1 <- Diagonal(100, c(rep(0,50),rep(1,50)))
Sigma <- mc_matrix_linear_predictor(tau = c(0.2, 0.15), Z = list(Z0,Z1))
y1 <- rnorm(100, mu1$mu, sd = sqrt(diag(Sigma)))
data <- data.frame("y1" = y1, "covariate" = covariate)

# Fitting using identity covariance function ---------------------------
fit2.id <- mcglm(linear_pred = c(y1 ~ covariate), 
                 matrix_pred = list("resp1" = list(Z0,Z1)), 
                 data = data)
summary(fit2.id)

# Case 3 - Longitudinal model using compound symmetry ------------------
covariate <- seq(-1,1, l = 200)
X <- model.matrix(~ covariate)
mu1 <- mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "identity")
Z0 <- Diagonal(200, 1)
Z1.temp <- Matrix(rep(1,10)%*%t(rep(1,10)))
Z1.list <- list()
for(i in 1:20){Z1.list[[i]] <- Z1.temp}
Z1 <- bdiag(Z1.list)
Sigma <- mc_matrix_linear_predictor(tau = c(0.2, 0.15), Z = list(Z0,Z1))
y1 <- as.numeric(rmvnorm(1, mean = mu1$mu, sigma = as.matrix(Sigma)))
data <- data.frame("y1" = y1, "covariate" = covariate)

# Fitting using identity covariance function ---------------------------
fit3.id <- mcglm(linear_pred = c(y1 ~ covariate), 
                 matrix_pred = list("resp1" = list(Z0,Z1)), data = data)
summary(fit3.id)

# Fitting using exponential-matrix covariance function -----------------
fit3.expm <- mcglm(linear_pred = c(y1 ~ covariate), 
                   matrix_pred = list("resp1" = list(Z0,Z1)),
                   covariance = "expm", data = data)
summary(fit3.expm)

# Case 4 - Logistic regression model -----------------------------------
covariate <- seq(-1,1, l = 250)
X <- model.matrix(~ covariate)
mu1 <- mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "logit")
Z0 <- Diagonal(250, 1)
y1 <- rbinom(250, prob = mu1$mu, size = 10)/10
data <- data.frame("y1" = y1, "covariate" = covariate)

# Logit link function --------------------------------------------------
fit4.logit <- mcglm(linear_pred = c(y1 ~ covariate), 
                    matrix_pred = list("resp1" = list(Z0)),
                    link = "logit", variance = "binomialP", 
                    Ntrial = list(rep(10,250)), data = data)
summary(fit4.logit)

# Probit link function -------------------------------------------------
fit4.probit <- mcglm(linear_pred = c(y1 ~ covariate), 
                     matrix_pred = list("resp1" = list(Z0)),
                     link = "probit", variance = "binomialP", 
                     Ntrial = list(rep(10,250)), data = data)
summary(fit4.probit)

# Cauchit link function ------------------------------------------------
fit4.cauchit <- mcglm(linear_pred = c(y1 ~ covariate), 
                      matrix_pred = list("resp1" = list(Z0)),
                      link = "cauchit", variance = "binomialP", 
                      Ntrial = list(rep(10,250)), data = data)
summary(fit4.cauchit)

# Cloglog link function ------------------------------------------------
fit4.cloglog <- mcglm(linear_pred = c(y1 ~ covariate), 
                      matrix_pred = list("resp1" = list(Z0)),
                      link = "cloglog", variance = "binomialP", 
                      Ntrial = list(rep(10,250)), data = data)
summary(fit4.cloglog)

# loglog link function -------------------------------------------------
fit4.loglog <- mcglm(linear_pred = c(y1 ~ covariate), 
                     matrix_pred = list("resp1" = list(Z0)),
                     link = "loglog", variance = "binomialP", 
                     Ntrial = list(rep(10,250)), data = data)
summary(fit4.loglog)

# Example 5 - Logistic regression with extra power parameter -----------
fit5 <- mcglm(linear_pred = c(y1 ~ covariate), 
              matrix_pred = list("resp1" = list(Z0)),
              link = "logit", variance = "binomialP", 
              Ntrial = list(rep(10,250)),
              power_fixed = list(FALSE), data = data)
summary(fit5)

# Example 6 - Logistic regression with two extra power parameters ------
# This model can be very hard to fit and require very carefull 
# initial values and tunning.
fit6 <- mcglm(linear_pred = c(y1 ~ covariate), 
              matrix_pred = list("resp1" = list(Z0)),
              link = "logit", variance = "binomialPQ", 
              Ntrial = list(rep(10,250)),
              power_fixed = list(FALSE), data = data,
              control_algorithm = list("method" = "chaser", 
                                       "tunning" = 0.1,
                                       "max_iter" = 1000, 
                                       "verbose" = FALSE))
summary(fit6)
plot(fit6, type = "algorithm")

# Case 7 - Gamma regression model --------------------------------------
covariate <- seq(-1,1, l = 100)
X <- model.matrix(~ covariate)
mu1 <- mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "log")
Z0 <- Diagonal(100, 1)
y1 <- rtweedie(100, mu = mu1$mu, power = 2, phi = 0.5)
data <- data.frame("y1" = y1, "covariate" = covariate)

# Initial values -------------------------------------------------------
list_initial = list()
list_initial$regression <- list("resp1" = c(1,0))
list_initial$power <- list("resp1" = c(2))
list_initial$tau <- list("resp1" = c(0.1))
list_initial$rho = 0

# Power parameter fixed ------------------------------------------------
fit7 <- mcglm(linear_pred = c(y1 ~ covariate), 
              matrix_pred = list("resp1" = list(Z0)),
              link = "log", variance = "tweedie", 
              power_fixed = list(TRUE),
              control_initial = list_initial, data = data)
summary(fit7)
plot(fit7, type = "algorithm")

# Estimating the power parameter ---------------------------------------
fit7.power <- mcglm(linear_pred = c(y1 ~ covariate), 
                    matrix_pred = list("resp1" = list(Z0)),
                    link = "log", variance = "tweedie",
                    control_initial = list_initial,
                    power_fixed = FALSE, data = data)
summary(fit7.power)

# Case 8 - Inverse Gaussian regression model ---------------------------
covariate <- seq(-2,2, l = 200)
X <- model.matrix(~ covariate)
mu1 <- mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "log")
Z0 <- Diagonal(200, 1)
y1 <- rtweedie(200, mu = mu1$mu, power = 3, phi = 0.5)
data <- data.frame("y1" = y1, "covariate" = covariate)

# Initial values list --------------------------------------------------
list_initial = list()
list_initial$regression <- list("resp1" = c(1,0))
list_initial$power <- list("resp1" = c(3))
list_initial$tau <- list("resp1" = c(0.1))
list_initial$rho = 0

# Power parameter fixed ------------------------------------------------
fit8 <- mcglm(linear_pred = c(y1 ~ covariate), 
              matrix_pred = list("resp1" = list(Z0)),
              link = "log", variance = "tweedie", data = data, 
              control_initial = list_initial)
summary(fit8)

# Estimating the power parameter ---------------------------------------
fit8.power <- mcglm(linear_pred = c(y1 ~ covariate), 
                    matrix_pred = list("resp1" = list(Z0)),
                    link = "log", variance = "tweedie", 
                    power_fixed = list(FALSE), data = data,
                    control_initial = list_initial)
summary(fit8.power)
plot(fit8.power, type = "algorithm")


# Case 9 - Poisson-Tweedie regression model ----------------------------
y1 <- rtweedie(200, mu = mu1$mu, power = 1.5, phi = 0.5)
data <- data.frame("y1" = y1, "covariate" = covariate)

fit9 <- mcglm(linear_pred = c(y1 ~ covariate), 
              matrix_pred = list("resp1" = list(Z0)),
              link = "log", variance = "tweedie", 
              power_fixed = list(FALSE), data = data,
              control_algorithm = list("method" = "chaser", 
                                       "tunning" = 1))
summary(fit9)
plot(fit9, type = "algorithm")

# Case 10 - Poisson regression model -----------------------------------
y1 <- rtweedie(200, mu = mu1$mu, power = 1, phi = 1)
data <- data.frame("y1" = y1, "covariate" = covariate)

fit10 <- mcglm(linear_pred = c(y1 ~ covariate), 
               matrix_pred = list("resp1" = list(Z0)),
               link = "log", variance = "tweedie",  
               power_fixed = list(FALSE), data = data,
               control_algorithm = list("method" = "rc", 
                                        "tunning" = 0.1))
summary(fit10)

# Case 11 - Poisson-Tweedie regression model (Neymann-Type A) ----------
# Neymann-Type A
y1 <- rtweedie(200, mu = mu1$mu, power = 1, phi = 1)
y1 <- rpois(200, lambda = y1)
data <- data.frame("y1" = y1, "covariate" = covariate)

fit11 <- mcglm(linear_pred = c(y1 ~ covariate), 
               matrix_pred = list("resp1" = list(Z0)),
               link = "log", variance = "poisson_tweedie", 
               power_fixed = list(TRUE), data = data)
summary(fit11)

# Case 12 - Poisson-Tweedie regression model (Negative Binomial) -------
y1 <- rtweedie(200, mu = mu1$mu, power = 2, phi = 1.5)
y1 <- rpois(200, lambda = y1)
data <- data.frame("y1" = y1, "covariate" = covariate)

# Initial values list --------------------------------------------------
list_initial = list()
list_initial$regression <- list("resp1" = c(1,0))
list_initial$power <- list("resp1" = c(2))
list_initial$tau <- list("resp1" = c(1))
list_initial$rho = 0

fit12 <- mcglm(linear_pred = c(y1 ~ covariate), 
               matrix_pred = list("resp1" = list(Z0)),
               link = "log", variance = "poisson_tweedie", 
               power_fixed = list(FALSE), data = data,
               control_initial = list_initial,
               control_algorithm = list("method" = "rc", 
                                        "tunning" = 0.2))
summary(fit12)

# Case 13 - Poisson-Tweedie regression model 
# (PIG - Poisson Inverse Gaussian)---------
y1 <- rtweedie(200, mu = mu1$mu, power = 3, phi = 0.1)
y1 <- rpois(200, lambda = y1)
data <- data.frame("y1" = y1, "covariate" = covariate)

# Initial values list --------------------------------------------------
list_initial = list()
list_initial$regression <- list("resp1" = c(1,0.8))
list_initial$power <- list("resp1" = c(3))
list_initial$tau <- list("resp1" = c(0.1))
list_initial$rho = 0

fit13 <- mcglm(linear_pred = c(y1 ~ covariate), 
               matrix_pred = list("resp1" = list(Z0)),
               link = "log", variance = "poisson_tweedie", data = data, 
               control_initial = list_initial,
               power_fixed = FALSE,
               control_algorithm = list("method" = "rc", 
                                        "tunning" = 1, 
                                        "max_iter" = 100))
summary(fit13)

# Case 14 - Poisson-Tweedie regression model (Pólya-Aeppli) ------------
y1 <- rtweedie(200, mu = mu1$mu, power = 1.5, phi = 1.5)
y1 <- rpois(200, lambda = y1)
data <- data.frame("y1" = y1, "covariate" = covariate)

fit14 <- mcglm(linear_pred = c(y1 ~ covariate), 
               matrix_pred = list("resp1" = list(Z0)),
               link = "log", variance = "poisson_tweedie", 
               power_fixed = FALSE, data = data)
summary(fit14)

# Methods --------------------------------------------------------------
# print
fit14

# vcov
vcov(fit14)

# confint
confint(fit14)

# summary
summary(fit14)

# anova
anova(fit14)

# Plot
plot(fit14)
plot(fit14, type = "algorithm")
plot(fit14, type = "partial_residuals")

# Residuals
hist(residuals(fit14, type = "raw")[,1])
hist(residuals(fit14, type = "pearson")[,1])
hist(residuals(fit14, type = "standardized")[,1])

# Fitted values
plot(as.numeric(fitted(fit14)) ~ data$y1)


