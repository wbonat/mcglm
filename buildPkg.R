##----------------------------------------------------------------------
## Script to build and verify the package.

if(!grepl(x=getwd(), pattern="/mcglm$")){
    stop("Move to /mcglm directory.")
}

##----------------------------------------------------------------------
## Packages.

library(devtools)

## Load the package (to make functiona available).
load_all()
search()

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

build(manual = TRUE, vignettes = FALSE)
# build the binary version for windows (not used)
# build_win()

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
if (file.exists(libTest)){
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
