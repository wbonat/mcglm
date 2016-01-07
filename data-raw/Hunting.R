##----------------------------------------------------------------------
## Prepare the data set.

setwd("/home/wagner/mcglm0.1/mcglm/data-raw")
Hunting <- read.table("Hunting.txt", header=TRUE)
Hunting <- Hunting[which(Hunting$Age == "A"),]
head(Hunting)
levels(Hunting$Sex) <- c("Female","Male")
Hunting$Alt <- as.factor(Hunting$Alt)
Hunting$Alt <- droplevels(Hunting$Alt)
levels(Hunting$Alt) <- c(1:4,5,5,5)
head(Hunting)
## dir.create("../data/")
save(Hunting, file = "../data/Hunting.RData")
rm(list = ls())
load("../data/Hunting.RData")
ls()
str(Hunting)

##----------------------------------------------------------------------
## Include in the @examples.
library(mcglm)
library(Matrix)
data(Hunting, package="mcglm")
formu <- OT ~ Method*Alt + Sex + Alt*poly(Month, 4)
Z0 <- Diagonal(dim(Hunting)[1],1)
fit <- mcglm(linear_pred = c(formu), matrix_pred = list(list(Z0)),
             link = c("log"), variance = c("poisson_tweedie"),
             power_fixed = c(FALSE),
             offset = list(log(Hunting$Offset)), data = Hunting)
summary(fit)
##----------------------------------------------------------------------
