## Prediction ------------------------------------------------------------------
## Prof. Wagner Hugo Bonat · LEG/UFPR ------------------------------------------
## Date: 16/11/2022 ------------------------------------------------------------

rm(list=ls())
## Loading extra packages
library(mcglm)
library(Matrix)
library(mvtnorm)

## Example 1 · Spatial data

x1 <- runif(5, 0, 1)
x2 <- runif(5, 0, 1)
coords <- expand.grid(x1, x2)

# Data set
dataset <- data.frame(x1 = coords$Var1, x2 = coords$Var2)

# Matrix linear predictor components
Z0 <- mc_id(dataset)[[1]]
Z1 <- as.matrix(1/dist(dataset, diag = TRUE, upper = TRUE))
Z1 <- Matrix(Z1)

# Covariance link function
C <- mc_build_omega(tau = c(10, 0.5), Z = list(Z0, Z1), covariance_link = "identity")
cov2cor(C$Omega)
image(C$Omega)

# Gaussian case
set.seed(123)
y <- as.numeric(rmvnorm(n = 1, mean = rep(0, dim(dataset)[1]), sigma = as.matrix(C$Omega)))
dataset$y <- y

## Suppose we have only the first 20 observations for fitting our model

# Fitting
tt <- mcglm(c(y ~ 1), matrix_pred = list(c(Z0[1:20,1:20], Z1[1:20, 1:20])),
            covariance = "identity", data = dataset[1:20,])
summary(tt)


# Prediction example: Suppose the last 5 observations are missing
dataset$y_NA <- dataset$y
dataset[21:25,]$y_NA <- NA

# Joint covariance matrix
C_joint <- mc_build_omega(tau = tt$Covariance, Z = list(Z0, Z1), covariance_link = "identity")
C_obs <- C_joint$Omega[1:20, 1:20]
C_cov <- C_joint$Omega[21:25, 1:20]
C_non_obs <- C_joint$Omega[21:25, 21:25]

# Prediction: Conditional mean
preditos <- tt$fitted[1:5] + C_cov%*%solve(C_obs)%*%(dataset$y[1:20] - tt$fitted[1:20])
preditos

dataset$y_NA[21:25] <- preditos
dataset[21:25,]

