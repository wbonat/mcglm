#=======================================================================
# Script to Check, Build and Distribute the `mcglm` Package
#
#                                                        mcglm Core Team
#=======================================================================

#-----------------------------------------------------------------------
# Check working directory.

switch(Sys.info()["user"],
       "wagner" = { NULL },
       "fernandomayer" = { NULL },
       "walmes" = { setwd("~/repos/mcglm/") },
       {
           if (basename(getwd()) != "mcglm") {
               stop("The working directory isn't /mcglm.")
           }
       })
cat(getwd(), "\n")

#-----------------------------------------------------------------------
# Packages.

library(devtools)
library(withr)
library(knitr)

# Load the package (to make functions available).
load_all()

# Show all exported objects.
ls("package:mcglm")
packageVersion("mcglm")

# How many objects in each class.
table(sapply(ls("package:mcglm"),
             function(x) class(eval(parse(text=x)))))

#-----------------------------------------------------------------------
# Check.

load_all()

# Create/update NAMESPACE, *.Rd files.
document()

# Check documentation.
check_man()

# Check functions, datasets, run examples, etc. Using cleanup = FALSE
# and check_dir = "../" will create a directory named mcglm.Rcheck
# with all the logs, manuals, figures from examples, etc.
check(cleanup = FALSE, manual = TRUE, vignettes = FALSE,
      check_dir = "../")

#-----------------------------------------------------------------------
# Build the package (it will be one directory up).

build(manual = TRUE, vignettes = TRUE)
# build the binary version for windows (not used)
# build_win()

#-----------------------------------------------------------------------
# Package vignette.
# Based on: http://r-pkgs.had.co.nz/vignettes.html

# Create the vignette template. Do just once.
# use_vignette("UniModels")
# use_vignette("functions_network")
# use_package(package = "networkD3", type = "Suggests")

build_vignettes()

# vignette()
# vignette("UniModels", package="mcglm")

#-----------------------------------------------------------------------
# Generate the README.md.

knit(input = "README.Rmd")

#-----------------------------------------------------------------------
# Examples.

# Run examples from all functions of the package
# run_examples()
# Run examples from a specific function
# dev_example("yscale.components.right")

#-----------------------------------------------------------------------
# Test installation 1: Install from the local .tar.gz.

libTest <- path.expand("~/R-test/")
if (file.exists(libTest)) {
    unlink(libTest, recursive = TRUE)
}
dir.create(path = libTest)

# Install with install.packages() from the .tar.gz. created by build().
pkg <- paste0("../mcglm_", packageVersion("mcglm"), ".tar.gz")

# Install in a temporary directory.
install.packages(pkg, repos = NULL, lib = libTest)
library(package = "mcglm", lib.loc = libTest)
packageVersion("mcglm")
ls("package:mcglm")

#-----------------------------------------------------------------------
# Test installation 2: Install from GitHub branches

list.files(path = libTest, recursive = TRUE)
unlink(paste0(libTest, "mcglm"), recursive = TRUE)

# Test using devtools::install_github().

## In order to make a "clean" test, and not modify a user's .libPaths(),
## we need to install devtools and all of its dependencies in the new
## libpath. The function withr::with_libpaths() creates a temporary
## libpath and install everything there. This is the only way to make
## install_github() to install a package in another libpath, without
## modifying the .libPaths().
## Install devtools in the new path
with_libpaths(new = libTest,
              install.packages("devtools", dependencies = TRUE))
## Install and test mcglm master
with_libpaths(new = libTest,
              install_github("wbonat/mcglm", ref = "master"))
library(package = "mcglm", lib.loc = libTest)
packageVersion("mcglm")
ls("package:mcglm")

## Install and test mcglm master
with_libpaths(new = libTest,
              install_github("wbonat/mcglm", ref = "devel"))
library(package = "mcglm", lib.loc = libTest)
packageVersion("mcglm")
ls("package:mcglm")

## Remove libTest
unlink(libTest, recursive = TRUE)

#-----------------------------------------------------------------------
# Sending package tarballs and manual to remote server to be
# downloadable.
# URL: http://www.leg.ufpr.br/~leg/mcglm/

pkg <- paste0("../mcglm_", packageVersion("mcglm"), ".tar.gz")
pkg.win <- paste0("../mcglm_", packageVersion("mcglm"), ".zip")

# Build the *.zip.
cmd.win <- paste("cd ../mcglm.Rcheck && zip -r", pkg.win, "mcglm")
system(cmd.win)

ntw <- "./data-raw/mcglm_network.html"
man <- "../mcglm.Rcheck/mcglm-manual.pdf"

## Send to LEG server
cmd <- paste("scp -P $PATAXOP", pkg, man, pkg.win, ntw,
             "leg@$PATAXO:~/public_html/mcglm/source")
system(cmd)
browseURL("http://www.leg.ufpr.br/~leg/mcglm/")

## Send to downloads/ folder, so it stays hosted on GitHub
dest <- "downloads/"
file.copy(c(pkg, pkg.win, man), dest, overwrite = TRUE)

#-----------------------------------------------------------------------
