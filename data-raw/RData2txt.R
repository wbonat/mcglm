##======================================================================
## Generate plain text files from the RData files for all datasets in
## the package.
##======================================================================

getwd()
f <- list.files(path="../data", pattern="*.RData")

sapply(f,
       FUN=function(f){
           load(paste0("../data/", f))
           g <- sub("\\.RData", "", f)
           txt <- paste0(g, ".txt")
           dataset <- eval(parse(text=g))
           cat(file=txt, sep="\n",
               "##----------------------------------------------------------------------",
               "## This dataset is part of mcglm package.",
               "## Visit http://git.leg.ufpr.br/wbonat/mcglm for details.",
               "##----------------------------------------------------------------------")
           write.table(x=dataset,
                       file=txt,
                       sep="\t", quote=FALSE, row.names=FALSE,
                       append=TRUE)
       })

##----------------------------------------------------------------------
## Uploading files to the public folder: www.leg.ufpr.br/~leg/mcglm.

## Port and IP.
u <- scan(n=2, what=character())

## Verifica o conteúdo do diretório /datasets.
cmd <- paste0("ssh leg@", u[2], " -p", u[1],
              " \"ls -la ~/public_html/mcglm/datasets\"")
system(cmd)

## By rsync.
cmd <- paste0("rsync -avzh --progress *.txt ",
              "-e \"ssh -p ", u[1], "\" leg@", u[2],
              ":~/public_html/mcglm/datasets/")
system(cmd)

url <- "http://www.leg.ufpr.br/~leg/mcglm/datasets"
browseURL(url)

##----------------------------------------------------------------------
