

# mcglm 0.0.1

[![build status](http://git.leg.ufpr.br/ci/projects/3/status.png?ref=master)](http://git.leg.ufpr.br/ci/projects/3?ref=master)
Build status for the stable version (`master` branch)

[![build status](http://git.leg.ufpr.br/ci/projects/3/status.png?ref=devel)](http://git.leg.ufpr.br/ci/projects/3?ref=devel)
Build status for the development version (`devel` branch)

The `mcglm` package fit multivariate covariance generalized linear models
(Bonat and Jorgensen, 2015).

## Introduction

`mcglm` fit multivariate covariance generalized linear models. It allows
use a different linear predictor for each response variable of a
multivariate response. The response variable can be continous or
dicrete, like counts and binary and also limited continuos ou
discrete/continuous inflated responses. The most important and relevant
feature is that many covariance structures can be used to model the
relations among variables.

This package is part of the Thesis of the first author.

## Download and install

### Linux/Mac

Use the `devtools` package (available from
[CRAN](http://cran-r.c3sl.ufpr.br/web/packages/devtools/index.html)) to
install automatically from this GitLab repository:


```r
library(devtools)
install_git("http://git.leg.ufpr.br/wbonat/mcglm.git")
```

Alternatively, download the package tarball: [mcglm_0.0.1.tar.gz][]
and run from a UNIX terminal (make sure you are on the container file
directory):


```
R CMD INSTALL -l /path/to/your/R/library mcglm_0.0.1.tar.gz
```

Or, inside an `R` session:


```
install.packages("mcglm_0.0.1.tar.gz", repos = NULL,
                 lib.loc = "/path/to/your/R/library",
                 dependencies = TRUE)
```

Note that `-l /path/to/your/R/library` in the former and `lib.loc =
"/path/to/your/R/library"` in the latter are optional. Only use it if
you want to install in a personal library, other than the standard R
library.

### Windows

Download Windows binary version: [mcglm_0.0.1.zip][] (**do not unzip
it under Windows**), put the file in your working directory, and from
inside `R`:


```
install.packages("mcglm_0.0.1.zip", repos = NULL,
                 dependencies = TRUE)
```

### Development version

By default, if you use `devtools::install_git()`, or download any of the
package tarball or Windows binary version, it will install the stable
version of the package (from the `master` branch of this repository).

If you want to install the development version, you can use

```r
library(devtools)
install_git("http://git.leg.ufpr.br/wbonat/mcglm.git", branch = "devel")
```

Note that the development version can contain bugs and other unknown
features, so use it at your own risk!

## Documentation

The reference manual in PDF can be found here: [mcglm-manual.pdf][]

## Contributing

This R package is develop using [`roxygen2`][] for documentation and
[`devtools`] to check and build. Also, we adopt the [Gitflow worflow][]
in this repository. Please, see the
[instructions for contributing](./contributing.md) to collaborate.

## License

This package is released under the
[GNU General Public License (GPL) v3.0][].

See [LICENSE](./LICENSE)

<!-- links -->



[GNU General Public License (GPL) v3.0]: http://www.gnu.org/licenses/gpl-3.0.html
[`roxygen2`]: https://github.com/klutometis/roxygen
[`devtools`]: https://github.com/hadley/devtools
[mcglm_0.0.1.tar.gz]: http://www.leg.ufpr.br/~leg/mcglm/source/mcglm_0.0.1.tar.gz
[mcglm_0.0.1.zip]: http://www.leg.ufpr.br/~leg/mcglm/source/mcglm_0.0.1.zip
[mcglm-manual.pdf]: http://www.leg.ufpr.br/~leg/mcglm/source/mcglm-manual.pdf
[Gitflow worflow]: http://nvie.com/posts/a-successful-git-branching-model/
