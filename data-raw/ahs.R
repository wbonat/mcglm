##----------------------------------------------------------------------
## Prepare the data set.

setwd("/home/walmes/GitLab/mcglm/data-raw")

ahs <- read.table("ahs.txt", header=TRUE, sep="\t")
str(ahs)

## save(ahs, file="../data/ahs.RData")

##----------------------------------------------------------------------
## Include in the @examples.

library(lattice)
library(latticeExtra)

data(ahs, package="mcglm")
str(ahs)

xt <- xtabs(~age+sex, data=ahs)
mosaicplot(xt)

xt <- xtabs(~age+chcond1, data=ahs)
mosaicplot(xt)

useOuterStrips(
    combineLimits(
        xyplot(Ndoc+Nndoc+Nadm+Nhosp+Nmed~age|sex,
               outer=TRUE, data=ahs,
               jitter.x=TRUE, amount=0.01,
               type=c("p", "a"),
               scales=list(y=list(relation="free")),
               ylab="Number or occurences",
               xlab="Age (years/100)")
    )
)

useOuterStrips(
    combineLimits(
        xyplot(Ndoc+Nndoc+Nadm+Nhosp+Nmed~income|sex,
               outer=TRUE, data=ahs,
               jitter.x=TRUE, amount=0.01,
               type=c("p", "a"),
               scales=list(y=list(relation="free")),
               ylab="Number or occurences",
               xlab="Age (years/100)")
    )
)

##----------------------------------------------------------------------
