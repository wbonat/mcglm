##----------------------------------------------------------------------
## Prepare the data set.

setwd("/home/wagner/mcglm0.1/mcglm/data-raw")
NewBorn <- read.table("NewBorn.txt", header=TRUE)
NewBorn <- NewBorn[,-16]
head(NewBorn)
names(NewBorn)[20] <- c("NBI")
## dir.create("../data/")
save(NewBorn, file = "../data/NewBorn.RData")
rm(list = ls())
load("../data/NewBorn.RData")
ls()
str(NewBorn)

##----------------------------------------------------------------------
## Include in the @examples.
library(mcglm)
library(Matrix)
data(NewBorn, package="mcglm")
formu <- SPO2 ~ Sex + APGAR1M + APGAR5M + PRE + HD + SUR
Z0 <- Diagonal(dim(NewBorn)[1],1)
fit <- mcglm(linear_pred = c(formu), matrix_pred = list(list(Z0)),
             link = c("logit"), variance = c("binomialP"),
             power_fixed = c(TRUE),
             data = NewBorn, 
             control_algorithm = list(verbose = FALSE, tunning = 0.5))
summary(fit)
##----------------------------------------------------------------------
