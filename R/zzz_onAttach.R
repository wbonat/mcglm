.onAttach <- function (lib, pkg) {
    pkg.info <- drop(read.dcf(
        file = system.file("DESCRIPTION", package = "mcglm"),
        fields = c("Package", "Title", "Version", "Date", "URL")
    ))
    dashes <- paste0(rep("----------", times = 7), collapse = "")
    packageStartupMessage(
        paste0(dashes, "\n  ",
               pkg.info["Package"], ": ", pkg.info["Title"], "\n\n  ",
               "For support, collaboration or bug report, visit: \n    ",
               pkg.info["URL"], "\n\n  ",
               pkg.info["Package"], " version ", pkg.info["Version"],
               " (build on ", pkg.info["Date"], ") is now loaded.\n",
               dashes)
    )
}
