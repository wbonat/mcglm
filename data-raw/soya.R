##----------------------------------------------------------------------
## Prepare the data set.

setwd("/home/wagner/mcglm0.1/mcglm/data-raw")
soya <- read.table("soya.txt", header=TRUE, dec = ",")
soya <- soya[,c(1,2,3,4,8,9,10)]
names(soya) <- c("pot","water","block","grain","seeds",
                 "viablepeas","totalpeas")
soya$pot <- as.factor(soya$pot)
soya$water <- as.factor(soya$water)
## dir.create("../data/")
save(soya, file = "../data/soya.RData")
rm(list = ls())
load("../data/soya.RData")
ls()
str(soya)

##----------------------------------------------------------------------
## Include in the @examples.
library(mcglm)
library(Matrix)
data(soya, package="mcglm")
formu <- grain ~ block + factor(water) * factor(pot)
Z0 <- mc_id(soya)
fit <- mcglm(linear_pred = c(formu), matrix_pred = list(Z0),
             data = soya)
anova(fit)
##----------------------------------------------------------------------
