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
#  install_git("http://git.leg.ufpr.br/wbonat/mcglm.git")

## ---- eval=FALSE, error=FALSE, message=FALSE, warning=FALSE-------
#  library(mcglm)
#  packageVersion("mcglm")

## ---- echo=FALSE, error=FALSE, message=FALSE, warning=FALSE-------
library(mcglm)
packageVersion("mcglm")

## -----------------------------------------------------------------
##----------------------------------------------------------------------
## Loadin the Australian Health Survey data.

data(ahs, package="mcglm")

## Object structure.
str(ahs)

## Descriptive measures.
summary(ahs)

##----------------------------------------------------------------------
## Frequency tables.

names(ahs)[c(1, 4:7, 10)]

par(mfrow=c(2,3))
## sapply(ahs[, c(1, 4:7, 10)],
##        FUN=function(x){
##            ## pie(table(x))
##            barplot(prop.table(table(x)))
##        })

barplot(prop.table(xtabs(~sex, data=ahs)),
        ylab="Sample proportion",
        xlab="Sex")

barplot(prop.table(xtabs(~levyplus, data=ahs)),
        ylab="Sample proportion",
        xlab="levyplus")

barplot(prop.table(xtabs(~freepoor, data=ahs)),
        ylab="Sample proportion",
        xlab="freepoor")

barplot(prop.table(xtabs(~freerepa, data=ahs)),
        ylab="Sample proportion",
        xlab="freerepa")

barplot(prop.table(xtabs(~illness, data=ahs)),
        ylab="Sample proportion",
        xlab="illness")

barplot(prop.table(xtabs(~chcond, data=ahs)),
        ylab="Sample proportion",
        xlab="chcond")
layout(1)

xt <- xtabs(~age+sex, data=ahs)
mosaicplot(xt)

xt <- xtabs(~age+chcond, data=ahs)
mosaicplot(xt)

xt <- xtabs(~sex+chcond, data=ahs)
mosaicplot(xt)

##----------------------------------------------------------------------

library(lattice)
library(latticeExtra)

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
               xlab="Income")
    )
)

useOuterStrips(
    combineLimits(
        xyplot(Ndoc+Nndoc+Nadm+Nhosp+Nmed~age|chcond,
               groups=sex, outer=TRUE, data=ahs,
               jitter.x=TRUE, amount=0.01,
               type=c("p", "a"),
               scales=list(y=list(relation="free")),
               ylab="Number or occurences",
               xlab="Age (years/100)")
    )
)


