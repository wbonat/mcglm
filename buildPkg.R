##----------------------------------------------------------------------
## Script to build and verify the package.

if (!grepl(x = getwd(), pattern = "/mcglm$")) {
    if (Sys.info()["user"] == "walmes") {
        setwd("~/GitLab/mcglm")
    }
    ## stop('Move to /mcglm directory.')
    cat(getwd(), "\n")
}

##----------------------------------------------------------------------
## Packages.

library(devtools)

## Load the package (to make functions available).
load_all()

## How many objects in each class.
table(sapply(ls("package:mcglm"),
             function(x) class(eval(parse(text=x)))))

## Create/update NAMESPACE, *.Rd files.
document()

## Check documentation.
check_doc()

## Check functions, datasets, run examples, etc. Using cleanup = FALSE
## and check_dir = "../" will create a directory named mcglm.Rcheck
## with all the logs, manuals, figures from examples, etc.
check(cleanup = FALSE, manual = TRUE, vignettes = FALSE,
      check_dir = "../")

##----------------------------------------------------------------------
## Show all exported objects.

ls("package:mcglm")
packageVersion("mcglm")

##----------------------------------------------------------------------
## Build the package (it will be one directory up).

build(manual = TRUE, vignettes = TRUE)
# build the binary version for windows (not used)
# build_win()

##----------------------------------------------------------------------
## Package vignette.
## Based on: http://r-pkgs.had.co.nz/vignettes.html

## Create the vignette template. Do just once.
## use_vignette("vignette-01")

build_vignettes()

## vignette()
## vignette("vignette-01", package="mcglm")

##----------------------------------------------------------------------
## Generate the README.md.

library(knitr)
knit(input = "README.Rmd") 

##----------------------------------------------------------------------
## Examples.

# Run examples from all functions of the package
# run_examples()
# Run examples from a specific function
# dev_example("yscale.components.right")

##----------------------------------------------------------------------
## Test installation.

## Test install with install.packages().
pkg <- paste0("../mcglm_", packageVersion("mcglm"), ".tar.gz")
install.packages(pkg, repos = NULL)

## Test using devtools::install_git().
libTest <- path.expand("~/R-test/")
if (file.exists(libTest)) {
    file.remove(libTest)
}
dir.create(path = libTest)
 
.libPaths(new = c(libTest, .libPaths())); .libPaths()

install_git(url = "http://git.leg.ufpr.br/wbonat/mcglm.git",
            branch = "devel")

library(mcglm)
packageVersion("mcglm")
ls("package:mcglm")

##----------------------------------------------------------------------
## Sending package tarballs and manual to remote server to be
## downloadable.

pkg.win <- paste0("../mcglm_", packageVersion("mcglm"), ".zip")
cmd.win <- paste("cd ../mcglm.Rcheck && zip -r", pkg.win, "mcglm")
system(cmd.win)

man <- "../mcglm.Rcheck/mcglm-manual.pdf"
cmd <- paste("scp -P $PATAXOP", pkg, man, pkg.win,
             "leg@$PATAXO:~/public_html/mcglm/source")
system(cmd)

##----------------------------------------------------------------------
