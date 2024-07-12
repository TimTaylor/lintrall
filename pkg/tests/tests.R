# Could a temporary directory get reused whilst interactively testing???
# just in case ...
library(lintr)    # nolint
library(lintrall) # nolint
dir <- tempdir()
target <- file.path(dir, ".lintr.R")
exists <- file.exists(target)
if (exists) {
    tmp <- tempfile()
    if (!file.copy(target, tmp))
        stop("Internal problem with tests. Please contact the maintainer.")
}
expected <- all_linters()
f <- use_all_linters(dir = dir, overwrite = TRUE)
x <- eval(parse(f))
if (!(setequal(x, expected) && length(expected) == length(x)))
    stop("Generated lintr file does not match output from all_linters.")
