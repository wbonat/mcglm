## ----setup, include=FALSE-----------------------------------------
##----------------------------------------------------------------------

library(knitr)

opts_chunk$set(
    dev.args=list(family="Palatino"))

options(width=68)

##----------------------------------------------------------------------

library(latticeExtra)
rm(list=ls())

## Color palette.
mycol <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
           "#FFFF33")
dput(mycol)

## Trellis graphical style.
ps <- list(
    box.rectangle=list(col=1, fill=c("gray70")),
    box.umbrella=list(col=1, lty=1),
    dot.symbol=list(col=1, pch=19),
    dot.line=list(col="gray50", lty=3),
    plot.symbol=list(col=1, cex=0.8),
    plot.line=list(col=1),
    plot.polygon=list(col="gray95"),
    superpose.line=list(col=mycol, lty=1),
    superpose.symbol=list(col=mycol, pch=1),
    superpose.polygon=list(col=mycol),
    strip.background=list(col=c("gray80","gray50"))
    )
trellis.par.set(ps)
## show.settings()


## ---- eval=FALSE--------------------------------------------------
#  library(devtools)
#  install_git("wbonat/mcglm")

## ---- eval=FALSE, error=FALSE, message=FALSE, warning=FALSE-------
#  library(mcglm)
#  packageVersion("mcglm")

## ---- echo=FALSE, error=FALSE, message=FALSE, warning=FALSE-------
library(mcglm)
packageVersion("mcglm")

## ---- warning = FALSE, message = FALSE----------------------------
# Loading extra packages
require(mcglm)
require(Matrix)
require(mvtnorm)
require(tweedie)

# Setting the seed
set.seed(2503)

# Fixed component
covariate <- seq(-1,1, l = 100)
X <- model.matrix(~ covariate)
mu1 <- mcglm::mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "identity")
# Random component
y1 <- rnorm(100, mu1$mu, sd = 0.5)

# Matrix linear predictor
Z0 <- Diagonal(100, 1)

# Data set
data <- data.frame("y1" = y1, "covariate" = covariate)

# Fit
fit1.id <- mcglm(linear_pred = c(y1 ~ covariate), 
                 matrix_pred = list(list(Z0)),
                 data = data)


## -----------------------------------------------------------------
print(methods(class = "mcglm"))

## -----------------------------------------------------------------
summary(fit1.id)

## -----------------------------------------------------------------
# Fit using inverse covariance link function 
fit1.inv <- mcglm(linear_pred = c(y1 ~ covariate), 
                  matrix_pred = list(list(Z0)),
                  covariance = "inverse", data = data)


## ---- message=FALSE, warning=FALSE--------------------------------
# Fit using expm covariance link function
fit1.expm <- mcglm(linear_pred = c(y1 ~ covariate), 
                   matrix_pred = list(list(Z0)),
                   covariance = "expm", data = data)

## -----------------------------------------------------------------
# Comparing estimates using different covariance link functions
cbind(coef(fit1.id)$Estimates,
      coef(fit1.inv)$Estimates,
      coef(fit1.expm)$Estimates)

# Applying the inverse transformation
c(coef(fit1.id)$Estimates[3],
  1/coef(fit1.inv)$Estimates[3],
  exp(coef(fit1.expm)$Estimates[3]))

## -----------------------------------------------------------------
# Mean model
covariate <- seq(-1,1, l = 100)
X <- model.matrix(~ covariate)
mu1 <- mcglm::mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "identity")

# Matrix linear predictor
Z0 <- Diagonal(100, 1)
Z1 <- Diagonal(100, c(rep(0,50),rep(1,50)))

# Covariance model
Sigma <- mcglm::mc_matrix_linear_predictor(tau = c(0.2, 0.15), 
                                           Z = list(Z0,Z1))
y1 <- rnorm(100, mu1$mu, sd = sqrt(diag(Sigma)))
data <- data.frame("y1" = y1, "covariate" = covariate)

fit2.id <- mcglm(linear_pred = c(y1 ~ covariate), 
                 matrix_pred = list(list(Z0,Z1)), 
                 data = data)

## -----------------------------------------------------------------
# Mean model
covariate <- seq(-1,1, l = 200)
X <- model.matrix(~ covariate)
mu1 <- mcglm::mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "identity")
# Covariance model
Z0 <- Diagonal(200, 1)
Z1.temp <- Matrix(rep(1,10)%*%t(rep(1,10)))
Z1.list <- list()
for(i in 1:20){Z1.list[[i]] <- Z1.temp}
Z1 <- bdiag(Z1.list)
Sigma <- mcglm::mc_matrix_linear_predictor(tau = c(0.2, 0.15), Z = list(Z0,Z1))

# Response variable
y1 <- as.numeric(rmvnorm(1, mean = mu1$mu, sigma = as.matrix(Sigma)))
data <- data.frame("y1" = y1, "covariate" = covariate)

# Fit
fit3.id <- mcglm(linear_pred = c(y1 ~ covariate), 
                 matrix_pred = list("resp1" = list(Z0,Z1)), data = data)

## -----------------------------------------------------------------
summary(fit3.id)

## -----------------------------------------------------------------
# Mean model
covariate <- seq(-1,1, l = 250)
X <- model.matrix(~ covariate)
mu1 <- mcglm::mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "logit")
# Covariance model
Z0 <- Diagonal(500, 1)

# Response variable
set.seed(123)
y1 <- rbinom(500, prob = mu1$mu, size = 10)/10

# Data set
data <- data.frame("y1" = y1, "covariate" = covariate)

## -----------------------------------------------------------------
# Fit
fit4.logit <- mcglm(linear_pred = c(y1 ~ covariate), 
                    matrix_pred = list(list(Z0)),
                    link = "logit", variance = "binomialP",
                    power_fixed = TRUE,
                    Ntrial = list(rep(10,250)), data = data)

## -----------------------------------------------------------------
fit4.cauchit <- mcglm(linear_pred = c(y1 ~ covariate), 
                      matrix_pred = list(list(Z0)),
                      link = "cauchit", variance = "binomialP", 
                      Ntrial = list(rep(10,250)), data = data)

## -----------------------------------------------------------------
fit4.logitP <- mcglm(linear_pred = c(y1 ~ covariate), 
                      matrix_pred = list(list(Z0)),
                      link = "logit", variance = "binomialP",
                      power_fixed = FALSE,
                      Ntrial = list(rep(10,250)), data = data)

## -----------------------------------------------------------------
fit4.logitPQ <- mcglm(linear_pred = c(y1 ~ covariate), 
                      matrix_pred = list(list(Z0)),
                      link = "logit", variance = "binomialPQ",
                      power_fixed = FALSE,
                      Ntrial = list(rep(10,250)), 
                      control_algorithm = list(tuning = 0.5),
                      data = data)

## -----------------------------------------------------------------
# Mean model
covariate <- seq(-2,2, l = 200)
X <- model.matrix(~ covariate)
mu <- mcglm::mc_link_function(beta = c(1,0.8), X = X, offset = NULL, 
                        link = "log")

# Covariance model
Z0 <- Diagonal(200, 1)

# Data
y1 <- rpois(200, lambda = mu$mu)
data <- data.frame("y1" = y1, "covariate" = covariate)

# Fit
fit.poisson <- mcglm(linear_pred = c(y1 ~ covariate), 
                    matrix_pred = list(list(Z0)),
                    link = "log", variance = "tweedie",
                    power_fixed = TRUE, data = data)

## -----------------------------------------------------------------
# Simulating negative binomial models
require(tweedie)
set.seed(1811)
y1 <- rtweedie(200, mu = mu$mu, power = 2, phi = 0.5)
y <- rpois(200, lambda = y1)
data <- data.frame("y1" = y, "covariate" = covariate)

fit.pt <- mcglm(linear_pred = c(y ~ covariate), 
               matrix_pred = list(list(Z0)),
               link = "log", variance = "poisson_tweedie", 
               power_fixed = FALSE, data = data)
summary(fit.pt)

