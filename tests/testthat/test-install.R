# The package must INSTALL, not merely load_all().
#
# `devtools::load_all()` never builds a NAMESPACE: it sources the R files and
# attaches the imports itself. So a NAMESPACE that `R CMD INSTALL` cannot parse is
# invisible to the whole suite — and one shipped. roxygen groups one package's
# imports into a single directive, and a STRING mixed with a BACKQUOTED SYMBOL
# (`@importFrom rlang ensym \`:=\``) makes parseNamespaceFile deparse every element,
# so the installer looked for an export literally named `` `:=` ``:
#
#   Error: object '`:=`' is not exported by 'namespace:rlang'
#
# Quoting it (`":="`) is the fix; this is the check that would have caught it.

test_that("the NAMESPACE names its rlang imports in a form R CMD INSTALL can parse", {
  # cheap, runs everywhere, and catches the exact shape of the defect
  ns <- readLines(testthat::test_path("..", "..", "NAMESPACE"), warn = FALSE)
  expect_false(any(grepl("`", ns, fixed = TRUE)),
               info = "a backquoted symbol in NAMESPACE makes the whole directive deparse")
  # ...and the import really is there, so the test cannot pass by it going missing
  expect_true(any(grepl('":="', ns, fixed = TRUE)))
})

test_that("R CMD INSTALL into a throwaway library succeeds and the copy loads", {
  skip_on_cran()
  skip_on_ci()
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  gate <- file.path(root, "inst", "gates", "fresh_install.R")
  skip_if_not(file.exists(gate), "the install gate is not in this tree")
  # slow (a full install), and it writes only under tempdir()
  skip_if(isTRUE(as.logical(Sys.getenv("MSENS_SKIP_INSTALL_GATE", "false"))),
          "MSENS_SKIP_INSTALL_GATE is set")

  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(c(gate, root)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  expect_true(is.null(status) || status == 0L,
              info = paste(utils::tail(out, 20), collapse = "\n"))
  expect_true(any(grepl("PASS: the package installs", out)))
})
