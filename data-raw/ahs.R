##----------------------------------------------------------------------
## Prepare the data set.

ahs <- read.table("data-raw/ahs.txt", header=TRUE, sep="\t")
str(ahs)

## save(ahs, file="../data/ahs.RData")

##----------------------------------------------------------------------
## Include in the @examples.
require(mcglm)
form1 <- Ndoc ~ income + age
form2 <- Nndoc ~ income + age
Z0 <- mc_id(ahs)
fit.ahd <- mcglm(linear_pred = c(form1, form2),
                 matrix_pred = list(Z0, Z0),
                 link = c("log","log"),
                 variance = c("poisson_tweedie","poisson_tweedie"),
                 data = ahs)

## dir.create("../data/")
save(ahs, file = "../data/ahs.RData")
rm(list = ls())
load("../data/ahs.RData")
ls()
str(ahs)

##----------------------------------------------------------------------
