## ----setup, include=FALSE-----------------------------------------
#-----------------------------------------------------------------------

library(knitr)

opts_chunk$set(
    dev.args=list(family="Palatino"))

options(width=68)

#-----------------------------------------------------------------------

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

## ---- warning = FALSE, message = FALSE----------------------------
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))

## ---- warning = FALSE, message = FALSE----------------------------
fit.glm <- glm(counts ~ outcome + treatment, family = quasipoisson)

