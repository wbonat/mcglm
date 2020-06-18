# Set of examples 2 - GLM examples -------------------------------------
# Author: Wagner Hugo Bonat LEG/IMADA ----------------------------------
# Date: 08/08/2015 -----------------------------------------------------
#-----------------------------------------------------------------------
rm(list=ls())

# Loading extra packages
require(mcglm)
require(Matrix)
# Case 1 ---------------------------------------------------------------
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))

# Orthodox Poisson model -----------------------------------------------
fit.glm <- glm(counts ~ outcome + treatment, family = poisson())
summary(fit.glm)

# Quasi-Poisson model via mcglm-----------------------------------------
Z0 <- Diagonal(dim(d.AD)[1],1)
fit.qglm <- mcglm(linear_pred = c(counts ~ outcome + treatment),
                  matrix_pred = list("resp1" = list(Z0)),
                  link = "log", variance = "tweedie", data = d.AD,
                  control_algorithm = list("verbose" = FALSE,
                                           "method" = "chaser",
                                           "tunning" = 0.8))
summary(fit.qglm)
cbind("mcglm" = round(coef(fit.qglm, type = "beta")$Estimates,5),
      "glm" = round(coef(fit.glm),5))
cbind("mcglm" = sqrt(diag(vcov(fit.qglm))),
      "glm" = c(sqrt(diag(vcov(fit.glm))),NA))
plot(fit.qglm)
plot(fit.qglm, type = "algorithm")

# Poisson-Tweedie model via mcglm---------------------------------------
list_initial = list()
list_initial$regression <- list("resp1" = coef(fit.glm) )
list_initial$power <- list("resp1" = c(1))
list_initial$tau <- list("resp1" = c(0.01))
list_initial$rho = 0
Z0 <- Diagonal(dim(d.AD)[1],1)

fit.pt <- mcglm(linear_pred = c(counts ~ outcome + treatment),
                matrix_pred = list("resp1" = list(Z0)),
                link = "log", variance = "poisson_tweedie",
                power_fixed = TRUE,
                data = d.AD, control_initial = list_initial,
                control_algorithm = list("correct" = TRUE,
                                         "verbose" = TRUE,
                                         "tol" = 1e-5,
                                         "max_iter" = 100,
                                         "method" = "chaser",
                                         "tunning" = 1))
summary(fit.pt)
cbind("mcglm" = round(coef(fit.pt, type = "beta")$Estimates,5),
      "glm" = round(coef(fit.glm),5))
cbind("mcglm" = sqrt(diag(vcov(fit.pt))),
      "glm" = c(sqrt(diag(vcov(fit.glm))),NA))

# This model is unsuitable for this data, note that the dispersion
# parameter is negative, indicating underdispersion.
# Which agrees with my quasi-Poisson model, but the glm function
# does not agree with this result. I have to understand this difference.

# Case 2 ---------------------------------------------------------------
# An example with offsets from Venables & Ripley (2002, p.189)

# Loading the data set
utils::data(anorexia, package = "MASS")

# Orthodox GLM fit -----------------------------------------------------
anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                family = gaussian, data = anorexia)
summary(anorex.1)

# Fitting by mcglm -----------------------------------------------------
Z0 <- Diagonal(dim(anorexia)[1],1)

fit.anorexia <- mcglm(linear_pred = c(Postwt ~ Prewt + Treat),
                      matrix_pred = list("resp1" = list(Z0)),
                      link = "identity", variance = "constant",
                      offset = list(anorexia$Prewt),
                      power_fixed = TRUE, data = anorexia,
                      control_algorithm = list("correct" = TRUE))
summary(fit.anorexia)

# Comparing the results ------------------------------------------------
cbind("mcglm" = round(coef(fit.anorexia, type = "beta")$Estimates,5),
      "glm" = round(coef(anorex.1),5))
cbind("mcglm" = sqrt(diag(vcov(fit.anorexia))),
      "glm" = c(sqrt(diag(vcov(anorex.1))),NA))


# Case 3 ---------------------------------------------------------------
# A Gamma example, from McCullagh & Nelder (1989, pp.300-2)
clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
fit.lot1 <- glm(lot1 ~ log(u), data = clotting,
                family = Gamma(link = "inverse"))
fit.lot2 <- glm(lot2 ~ log(u), data = clotting,
                family = Gamma(link = "inverse"))
summary(fit.lot1)

# Initial values -------------------------------------------------------
list_initial = list()
list_initial$regression <- list("resp1" = coef(fit.lot1))
list_initial$power <- list("resp1" = c(2))
list_initial$tau <- list("resp1" = summary(fit.lot1)$dispersion)
list_initial$rho = 0
Z0 <- Diagonal(dim(clotting)[1],1)

# Fitting --------------------------------------------------------------
fit.lot1.mcglm <- mcglm(linear_pred = c(lot1 ~ log(u)),
                        matrix_pred = list("resp1" = list(Z0)),
                        link = "inverse", variance = "tweedie",
                        data = clotting, control_initial = list_initial)
summary(fit.lot1.mcglm)

cbind("mcglm" = round(coef(fit.lot1.mcglm, type = "beta")$Estimates,5),
      "glm" = round(coef(fit.lot1),5))
cbind("mcglm" = sqrt(diag(vcov(fit.lot1.mcglm))),
      "glm" = c(sqrt(diag(vcov(fit.lot1))),NA))

# Initial values -------------------------------------------------------
list_initial$regression <- list("resp1" = coef(fit.lot2))
list_initial$tau <- list("resp1" = c(var(1/clotting$lot2)))

# Fitting --------------------------------------------------------------
fit.lot2.mcglm <- mcglm(linear_pred = c(lot2 ~ log(u)),
                        matrix_pred = list("resp2" = list(Z0)),
                        link = "inverse", variance = "tweedie",
                        data = clotting,
                        control_initial = list_initial)
summary(fit.lot2.mcglm)

cbind("mcglm" = round(coef(fit.lot2.mcglm, type = "beta")$Estimates,5),
      "glm" = round(coef(fit.lot2),5))
cbind("mcglm" = sqrt(diag(vcov(fit.lot2.mcglm))),
      "glm" = c(sqrt(diag(vcov(fit.lot2))),NA))

# Bivariate Gamma model-------------------------------------------------
list_initial = list()
list_initial$regression <- list("resp1" = coef(fit.lot1),
                                "resp2" = coef(fit.lot2))
list_initial$power <- list("resp1" = c(2), "resp2" = c(2))
list_initial$tau <- list("resp1" = c(0.00149), "resp2" = c(0.001276))
list_initial$rho = 0.80
Z0 <- Diagonal(dim(clotting)[1],1)

fit.joint.mcglm <- mcglm(linear_pred = c(lot1 ~ log(u), lot2 ~ log(u)),
                         matrix_pred = list(list(Z0), list(Z0)),
                         link = c("inverse", "inverse"),
                         variance = c("tweedie", "tweedie"),
                         data = clotting, control_initial = list_initial,
                         control_algorithm = list(correct = TRUE,
                                                  method = "chaser",
                                                  verbose = TRUE,
                                                  tuning = 1,
                                                  max_iter = 100))
summary(fit.joint.mcglm)
plot(fit.joint.mcglm, type = "algorithm")
plot(fit.joint.mcglm)

# Bivariate Gamma model + log link function ----------------------------
list_initial = list()
list_initial$regression <- list("resp1" = c(log(mean(clotting$lot1)),0),
                                "resp2" = c(log(mean(clotting$lot2)),0))
list_initial$power <- list("resp1" = c(2), "resp2" = c(2))
list_initial$tau <- list("resp1" = 0.023, "resp2" = 0.024)
list_initial$rho = 0
Z0 <- Diagonal(dim(clotting)[1],1)

fit.joint.log <- mcglm(linear_pred = c("resp1" = lot1 ~ log(u),
                                       "resp2" = lot2 ~ log(u)),
                       matrix_pred = list(list(Z0),list(Z0)),
                       link = c("log", "log"),
                       variance = c("tweedie", "tweedie"),
                       data = clotting,
                       control_initial = list_initial)
summary(fit.joint.log)
plot(fit.joint.mcglm, type = "algorithm")
plot(fit.joint.mcglm)

# Case 4 - Binomial regression models ----------------------------------
require(MASS)
data(menarche)
head(menarche)
data <- data.frame("resp" = menarche$Menarche/menarche$Total,
                   "Ntrial" = menarche$Total,
                   "Age" = menarche$Age)

# Orthodox logistic regression model -----------------------------------
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age,
              family=binomial(logit), data=menarche)

# Fitting --------------------------------------------------------------
Z0 <- Diagonal(dim(data)[1],1)

fit.logit <- mcglm(linear_pred = c(resp ~ Age),
                   matrix_pred = list("resp1" = list(Z0)),
                   link = "logit", variance = "binomialP",
                   Ntrial = list(data$Ntrial), data = data)

summary(fit.logit)
plot(fit.logit, type = "algorithm")
plot(fit.logit)

# Fitting with extra power parameter -----------------------------------
fit.logit.power <- mcglm(linear_pred = c(resp ~ Age),
                         matrix_pred = list(list(Z0)),
                         link = "logit", variance = "binomialP",
                         Ntrial = list(data$Ntrial),
                         power_fixed = FALSE, data = data)
summary(fit.logit.power)
plot(fit.logit.power, type = "algorithm")
plot(fit.logit.power)

# All methods ----------------------------------------------------------
# print method
fit.logit.power
# coef method
coef(fit.logit.power)
# confint method
confint(fit.logit.power)
# vcov method
vcov(fit.logit.power)
# summary method
summary(fit.logit.power)
# anova method
anova(fit.logit.power)
# Fitted method
fitted(fit.logit.power)
# Residuals method
residuals(fit.logit.power)
# Plot method
plot(fit.logit.power)
plot(fit.logit.power, type = "algorithm")
plot(fit.logit.power, type = "partial_residuals")
# End
