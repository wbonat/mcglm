library(devtools)
library(plyr)

## Load the package (to make functions available).
load_all("~/GitLab/mcglm/")

rm(list=ls())
L <- lapply(list.files(path = "~/GitLab/mcglm/R", pattern = "*.R"),
            function(x) {
                a <- ls(pos = ".GlobalEnv")
                source(sprintf("~/GitLab/mcglm/R/%s", x))
                b <- setdiff(x = ls(pos = ".GlobalEnv"), y = a)
                if (length(b) == 0) {
                    return(NULL)
                } else {
                    return(data.frame(file = x, fun = b,
                                      stringsAsFactors = FALSE))
                }
            })

da <- ldply(L)
da <- da[, rev(1:ncol(da))]

## Quantas funções cada arquivo tem.
## cbind(sort(xtabs(~file, data = da), decreasing = TRUE))

##----------------------------------------------------------------------
## Regex para bater com conjunto das funções existentes.

funs <- unique(da$fun)
funs <- funs[!funs == "mcglm"]
regex <- paste0("^.*(", paste(funs, collapse = "|"), ").*$")
funs <- da$fun

##----------------------------------------------------------------------
## NetWork.

## https://cran.r-project.org/web/packages/networkD3/index.html
## https://christophergandrud.github.io/networkD3/
library(networkD3)

funRel <- function(fx) {
    fx
    bdy <- capture.output(body(fx))
    funscalled <- gsub(pattern = regex,
                       x = grep(pattern = regex, x = bdy, value = TRUE),
                       replacement = "\\1")
    if (length(funscalled)) {
        return(data.frame(parent = fx, child = funscalled,
                          stringsAsFactors = FALSE))
    } else {
        return(data.frame(parent = fx, child = NA,
                           stringsAsFactors = FALSE))
    }
}

a <- do.call(rbind, lapply(funs, funRel))
a <- arrange(a, parent, child)
a <- transform(a,
               child = ifelse(is.na(child), parent, child))
a <- unique(a)

## simpleNetwork(a, nodeColour = "black", opacity = 0.9,
##               textColour = "black", charge = -300, fontSize = 12,
##               fontFamily = "inconsolata")

##----------------------------------------------------------------------
## Com cores.

fv <- factor(c(a$parent, a$child))

## IMPORTANT: a codificação deve começar em 0!
a <- transform(a,
               "source" =
                   as.integer(factor(parent, levels = levels(fv))) - 1,
               "target" =
                   as.integer(factor(child, levels = levels(fv))) - 1
               )
a$value <- 2

b <- data.frame(name = levels(fv))
b$group <- as.integer(factor(
    da$file[match(x = da$fun, table = as.character(b$name))]))
## b$group <- c(table(a$parent))
b$size <- 18

## Cores indicam o arquivo em que estão.
b$group <- as.integer(factor(
    merge(b, da, by.x = "name", by.y = "fun", all.x = TRUE)$file))

ntw <- forceNetwork(
    Links = a,
    Source = "source",
    Target = "target",
    Value = "value",
    Nodes = b,
    NodeID = "name",
    Group = "group",
    charge = -300,
    linkDistance = 80,
    linkColour = "black",
    opacity = 0.9,
    opacityNoHover = 1,
    fontSize = 12,
    fontFamily = "inconsolata")
ntw

saveNetwork(network = ntw, file = "mcglm_network.html")

##----------------------------------------------------------------------
