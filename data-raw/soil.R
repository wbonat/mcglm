##----------------------------------------------------------------------
## Prepare the data set.

setwd("/home/wagner/mcglm0.1/mcglm/data-raw")
soil <- read.table("soil", header=TRUE)
idx <- c(1,2,4,5,6,7,9,10,11)
soil <- soil[,idx]
names(soil) <- c("COORD.X","COORD.Y","SAND","SILT","CLAY","PHWATER",
                 "CA","MG","K")

head(soil)
## dir.create("../data/")
save(soil, file = "../data/soil.RData")
rm(list = ls())
load("../data/soil.RData")
ls()
str(soil)

##----------------------------------------------------------------------
## Include in the @examples.
library(mcglm)
library(Matrix)
data(soil, package="mcglm")
formu <- CA ~ COORD.X*COORD.Y + SAND + SILT + CLAY + PHWATER
Z0 <- Diagonal(dim(soil)[1],1)
fit <- mcglm(linear_pred = c(formu), matrix_pred = list(list(Z0)),
             link = c("log"), variance = c("tweedie"),
             covariance = c("inverse"),
             control_algorithm = list(verbose = FALSE, max_iter = 100,
                                      tunning = 1),
             power_fixed = c(FALSE), data = soil)
summary(fit)
##----------------------------------------------------------------------
