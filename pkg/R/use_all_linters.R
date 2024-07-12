# -------------------------------------------------------------------------
#' Setup a lintr configuration file with all linters explicitly enabled
#'
# -------------------------------------------------------------------------
#' @param ... Not currently used.
#'
#' @param dir Directory to create the `.lintr.R` file within.
#'
#' @param overwrite Should an existing `.lintr.R` file be overwritten.
#'
# -------------------------------------------------------------------------
#' @examples
#' dir <- tempdir()
#' f <- try(use_all_linters(dir = dir))
#' if (!inherits(f, "try-error")) {
#'     unlink(f)
#' }
#'
# -------------------------------------------------------------------------
#' @importFrom lintr available_linters
#' @importFrom tools Rd_db Rd2txt
# -------------------------------------------------------------------------
#' @export
use_all_linters <- function(..., dir = ".", overwrite = FALSE) {

    # TODO - changing lint defaults / excluding lints
    # TODO - can we get tools:::.Rd_get_section exported?

    # Create directory if it does not already exist
    dir <- normalizePath(dir)
    if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE))
        stop(sprintf("Unable to create directory %s", dir))

    # Check for pre-existing .lintr.R and onnly overwrite if explicitly stated
    filename <- file.path(dir, ".lintr.R")
    if (file.exists(filename) && !overwrite)
        stop("`.lintR.R` already exists. Set `overwrite = TRUE` to rewrite")

    # get the names of available linters
    names <- .subset2(available_linters(), "linter")

    # Get the functions and their signatures out of lintr
    namespace <- loadNamespace("lintr")
    call <- lapply(names, function(x) c(as.symbol(x), formals(x, namespace)))
    call <- vapply(call, function(x) deparse1(as.call(x)), FUN.VALUE = "")

    # Now process the Rd files for both the descriptions and argument
    rddb <- Rd_db("lintr")
    names(rddb) <- sub(".Rd$", "", names(rddb), perl = TRUE)
    rddb <- rddb[names]

    f <- tempfile()
    on.exit(unlink(f))
    txt <- vector("list", length(rddb))

    # TODO - get this exported!
    .get_section <- loadNamespace("tools")[[".Rd_get_section"]]
    for (i in seq_along(rddb)) {
        # Pull out both description and arguments sections. This seemed to work
        # better than trying to handle one at a time. I think this is down to
        # linters without arguments.
        rd <- .mapply(
            # this is not exported (can we ask for it to be?)
            function(x) .get_section(rddb[[i]], x),
            dots = list(x = c("description", "arguments")),
            MoreArgs = NULL
        )

        description <- rd[1L]
        Rd2txt(description, out = f, fragment = TRUE, underline_titles = FALSE)
        description <- readLines(f)

        arguments <- rd[2L]
        # not every linter has arguments so only handle those that do
        if (lengths(arguments) != 0L) {
            Rd2txt(arguments, out = f, fragment = TRUE, underline_titles = FALSE)
            arguments <- readLines(f)
            description <- c(description, arguments)
        }

        txt[[i]] <- c(description)
    }

    # Comment out the documentation and add some indentation
    txt <- lapply(txt, function(x) paste("    #", x, collapse = "\n"))

    # interleave the documentation and calls
    cat("linters <- list(\n", file = filename)
    for (i in seq_along(call)) {
        cat(txt[[i]], file = filename, append = TRUE)
        tmp <- sprintf("\n    %s%s", call[[i]], if (i == length(call)) "\n)" else ",\n\n")
        cat(tmp, file = filename, append = TRUE)
    }

    # allow for generate_all_linters() |> file.edit()
    invisible(filename)
}
