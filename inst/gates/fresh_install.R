#!/usr/bin/env Rscript
#
# GATE: the package actually INSTALLS, and loads from the installed copy.
#
#   Rscript inst/gates/fresh_install.R [pkg_root]
#
# Exit codes: 0 installed and loaded · 1 FAILED.
#
# Why this exists. `devtools::load_all()` never builds a NAMESPACE — it sources the
# R files and attaches the imports itself — so a NAMESPACE that R CMD INSTALL cannot
# parse is invisible to 2,392 passing tests. One did ship:
#
#   importFrom(rlang, "%||%", `:=`, ensym)
#
# roxygen groups the imports of one package into a single directive, and a STRING
# mixed with a BACKQUOTED SYMBOL makes parseNamespaceFile deparse every element, so
# the installer looked for an export literally named `` `:=` `` and refused:
#   "object '`:=`' is not exported by 'namespace:rlang'".
# The fix is in the roxygen source (`@importFrom rlang ensym ":="`, quoted); this is
# the check that would have caught it.
#
# The library is a THROWAWAY under tempdir(). Nothing is written to the user's own
# library, ever: this package is developed beside a release that depends on the
# installed copy staying where it is.

args <- commandArgs(trailingOnly = TRUE)
self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
root <- if (length(args) >= 1) args[1] else if (!is.na(self))
  normalizePath(file.path(dirname(self), "..", ".."), mustWork = FALSE) else "."

say <- function(...) cat(..., "\n", sep = "")
die <- function(...) { say("FAILED:  ", ...); quit(save = "no", status = 1) }

if (!file.exists(file.path(root, "DESCRIPTION")))
  die("no DESCRIPTION at ", root)
say("package: ", root)

lib <- file.path(tempdir(), paste0("msens_freshlib_", as.integer(Sys.time())))
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(lib, recursive = TRUE), add = TRUE)
say("throwaway library: ", lib)

# --no-test-load: we load it ourselves below, from this library only, so a failure
# is reported here rather than buried in the installer's own output
out <- suppressWarnings(system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-multiarch", "--no-test-load",
    paste0("--library=", shQuote(lib)), shQuote(root)),
  stdout = TRUE, stderr = TRUE))
status <- attr(out, "status")
bad <- grep("is not exported by|ERROR|cannot be opened|unable to load", out, value = TRUE)
if (!is.null(status) && status != 0L || length(bad)) {
  say("R CMD INSTALL output:")
  cat(paste0("  ", utils::tail(out, 40)), sep = "\n")
  die("R CMD INSTALL did not succeed cleanly",
      if (length(bad)) paste0("\n         first complaint: ", bad[1]) else "")
}
say("R CMD INSTALL: ok")

# load from THAT library and nowhere else, so a copy already on .libPaths() cannot
# stand in for the one just built. Via a FILE rather than `-e`: deparse() of a path
# vector wraps across lines and the shell then sees a half-finished call.
loader <- file.path(lib, "load_check.R")
writeLines(c(
  sprintf('.libPaths(%s)', deparse(lib, width.cutoff = 500L)),
  '.libPaths(c(.libPaths(), .Library))',
  'suppressMessages(library(msens))',
  'cat("loaded msens", as.character(utils::packageVersion("msens")), "\\n")',
  'stopifnot(is.function(msens::cells_in_polygon_grid),',
  '          is.function(msens::place_encode),',
  '          is.function(msens::app_bundle_build))',
  'cat("exports visible; fixtures:",',
  '    length(list.files(system.file("fixtures", "places", package = "msens"))), "\\n")'),
  loader)
out2 <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                 shQuote(loader), stdout = TRUE, stderr = TRUE))
if (!is.null(attr(out2, "status")) && attr(out2, "status") != 0L) {
  cat(paste0("  ", out2), sep = "\n")
  die("library(msens) failed from the freshly installed copy")
}
cat(paste0("  ", out2), sep = "\n")

say("\nPASS: the package installs into a clean library and loads from it.")
quit(save = "no", status = 0)
