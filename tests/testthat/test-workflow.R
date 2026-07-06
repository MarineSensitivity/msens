# tests for the msens: frontmatter -> targets generator in R/workflow.R

# write a minimal .qmd with a `msens:` frontmatter block
.write_qmd <- function(dir, file, target, type, deps, output) {
  dep_yaml <- if (length(deps) == 0) "[]" else paste0("[", paste(deps, collapse = ", "), "]")
  writeLines(c(
    "---",
    sprintf('title: "%s"', target),
    "msens:",
    sprintf("  target_name: %s", target),
    sprintf("  workflow_type: %s", type),
    sprintf("  dependency: %s", dep_yaml),
    sprintf("  output: %s", output),
    "---",
    "", "```{r}", "1 + 1", "```"),
    file.path(dir, file))
}

test_that("parse_qmd_frontmatter reads msens: blocks and skips notebooks without one", {
  d <- withr::local_tempdir()
  .write_qmd(d, "ingest_a.qmd", "ingest_a", "ingest", character(0), "out/a.json")
  .write_qmd(d, "merge_b.qmd",  "merge_b",  "merge",  "ingest_a",   "out/b.json")
  writeLines(c("---", 'title: "no block"', "---", "", "text"),
             file.path(d, "explore_z.qmd"))  # no msens: → excluded

  wf <- parse_qmd_frontmatter(d)
  expect_equal(nrow(wf), 2)
  expect_setequal(wf$target_name, c("ingest_a", "merge_b"))
  expect_equal(wf$dependency[[which(wf$target_name == "merge_b")]], "ingest_a")
  expect_equal(wf$dependency[[which(wf$target_name == "ingest_a")]], character(0))
})

test_that("build_targets_list emits one file target per notebook with DAG edges", {
  skip_if_not_installed("targets")
  d <- withr::local_tempdir()
  .write_qmd(d, "ingest_a.qmd", "ingest_a", "ingest", character(0), "out/a.json")
  .write_qmd(d, "merge_b.qmd",  "merge_b",  "merge",  "ingest_a",   "out/b.json")

  tl <- build_targets_list(d, verbose = FALSE)
  expect_length(tl, 2)
  expect_true(all(vapply(tl, inherits, logical(1), "tar_target")))
  nms <- vapply(tl, function(t) t$settings$name, character(1))
  expect_setequal(nms, c("ingest_a", "merge_b"))

  # merge_b's command references the bare symbol ingest_a (the DAG edge) + render
  cmd <- deparse(tl[[which(nms == "merge_b")]]$command$expr)
  expect_true(any(grepl("ingest_a", cmd)))
  expect_true(any(grepl("quarto_render", cmd)))
})

test_that("build_targets_list errors on a dependency that names no defined target", {
  skip_if_not_installed("targets")
  d <- withr::local_tempdir()
  .write_qmd(d, "merge_b.qmd", "merge_b", "merge", "ingest_missing", "out/b.json")
  expect_error(build_targets_list(d, verbose = FALSE), "undefined target")
})

test_that("[auto] dependency resolves to all grid + ingest targets", {
  skip_if_not_installed("targets")
  d <- withr::local_tempdir()
  .write_qmd(d, "grid.qmd",     "build_hex_grid", "grid",    character(0),                 "out/g.json")
  .write_qmd(d, "ingest_a.qmd", "ingest_a",       "ingest",  "build_hex_grid",             "out/a.json")
  .write_qmd(d, "release.qmd",  "release",        "release", "auto",                       "out/r.json")

  tl  <- build_targets_list(d, verbose = FALSE)
  nms <- vapply(tl, function(t) t$settings$name, character(1))
  rel <- tl[[which(nms == "release")]]
  cmd <- paste(deparse(rel$command$expr), collapse = " ")
  expect_true(grepl("build_hex_grid", cmd))
  expect_true(grepl("ingest_a", cmd))
})
