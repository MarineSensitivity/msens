#!/usr/bin/env Rscript
#
# GATE: build the whole `{ver}/app/` bundle from a REAL release and check it.
#
#   Rscript inst/gates/app_bundle_smoke.R [db] [ver] [dir_out]
#   MSENS_GATE_DB=… MSENS_GATE_VER=v7 Rscript inst/gates/app_bundle_smoke.R
#   MSENS_SMOKE_TILES=0   structural pass only (skip the wide cell tiles)
#
# Exit codes: 0 every check held · 1 FAILED · 77 SKIPPED (no database here).
#
# Why this exists. The synthetic databases in tests/testthat/helper-synth.R are
# faithful about SHAPE and silent about SCALE and about the values a real release
# actually holds. The first query against v7 found two defects that 2,392 green
# tests had not:
#
#   * `taxon.taxon_id` is a DOUBLE there, so `CAST(... AS VARCHAR)` published
#     "22725044.0" on all 16,153 rows;
#   * 2,354 of 14,501 edges came back all-NA, because a taxon with no merged model
#     makes `mdl_key != key` NA and `d[NA, ]` INJECTS a row rather than dropping it.
#
# Both now have unit tests and fixtures. This gate is what keeps the class of defect
# — "true of the real release, absent from the fixture" — from needing a third
# discovery.
#
# READ-ONLY on the release; everything written goes under tempdir().

args <- commandArgs(trailingOnly = TRUE)
db  <- if (length(args) >= 1) args[1] else Sys.getenv("MSENS_GATE_DB",
         "~/_big/msens/derived/v9/sdm.duckdb")
ver <- if (length(args) >= 2) args[2] else Sys.getenv("MSENS_GATE_VER", "v9")
db  <- path.expand(db)

say  <- function(...) cat(..., "\n", sep = "")
skip <- function(...) { say("SKIPPED: ", ...); quit(save = "no", status = 77) }
if (!file.exists(db))
  skip("no database at ", db, "\n         pass a path, or set MSENS_GATE_DB.")

self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
root <- if (!is.na(self))
  normalizePath(file.path(dirname(self), "..", ".."), mustWork = FALSE) else "."
if (file.exists(file.path(root, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  suppressMessages(pkgload::load_all(root, quiet = TRUE))
  say("msens: source tree ", root)
} else {
  suppressMessages(library(msens)); say("msens: installed")
}
suppressMessages({library(DBI); library(duckdb)})

dir_out <- if (length(args) >= 3) args[3] else
  file.path(tempdir(), paste0("app_smoke_", ver))
unlink(dir_out, recursive = TRUE)
tiles_on <- !identical(Sys.getenv("MSENS_SMOKE_TILES", "1"), "0")

con <- DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
say("database: ", db, "   version: ", ver, "   tiles: ", if (tiles_on) "on" else "OFF")

fail <- character()
chk <- function(ok, label, detail = "") {
  say(sprintf("  %-58s %s%s", label, if (isTRUE(ok)) "OK" else "FAIL",
              if (nzchar(detail)) paste0("  ", detail) else ""))
  if (!isTRUE(ok)) fail <<- c(fail, label)
  invisible(ok)
}

t0 <- Sys.time()
m  <- manifest_build(con, ver, base = atlas_base_url())
b  <- app_bundle_build(con, ver, dir_out, manifest = m, cell_tiles = tiles_on)
say(sprintf("built in %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

say("\n-- structure ----------------------------------------------------------")
chk(!is.null(b$boot$id_field) && b$boot$id_field %in% c("mdl_seq", "mdl_key"),
    "boot.json carries id_field", b$boot$id_field %||% "(absent)")
chk(identical(b$boot$id_field, m$id_field), "id_field agrees with the manifest")
chk(length(b$boot$tables) > 0, "boot.json lists its tables",
    paste(names(b$boot$tables), collapse = ", "))
chk(b$n_taxa > 0, "taxon.parquet has rows", format(b$n_taxa))
chk(b$n_shards > 0 && b$n_alias > 0, "taxon/ and alias/ shards exist",
    sprintf("%d / %d", b$n_shards, b$n_alias))
chk(!length(b$failed), "every stage completed",
    if (length(b$failed)) paste(names(b$failed), collapse = ", ") else "")

say("\n-- the word `value` never appears in a published object ---------------")
jsons <- list.files(dir_out, "[.]json$", recursive = TRUE, full.names = TRUE)
hits <- Filter(function(f) any(grepl('"value"', readLines(f, warn = FALSE), fixed = TRUE)),
               jsons)
chk(!length(hits), "no published JSON contains \"value\"",
    if (length(hits)) basename(hits[1]) else sprintf("%d files", length(jsons)))

say("\n-- taxon ids and edges (what the real releases found) -----------------")
tx <- app_taxon_table(con)
ok_ids <- all(is.na(tx$taxon_id) | grepl("^[0-9]+$", tx$taxon_id))
chk(ok_ids, "every taxon_id is digits only (no trailing .0)",
    if (ok_ids) sprintf("%d rows", nrow(tx)) else
      paste(utils::head(tx$taxon_id[!grepl("^[0-9]+$", tx$taxon_id)], 3), collapse = " "))
e <- msens:::.app_edges(con)
chk(sum(!stats::complete.cases(e)) == 0L, "no all-NA edge rows",
    sprintf("%d edges", nrow(e)))
keys <- unlist(app_taxa(con, ver)$key)
shard_keys <- unlist(lapply(list.files(file.path(dir_out, "taxon"), full.names = TRUE),
                            function(f) names(jsonlite::fromJSON(f, simplifyVector = FALSE)$taxa)))
chk(setequal(shard_keys, keys) && !anyDuplicated(shard_keys),
    "taxon shard keys are exactly the taxa.json set",
    sprintf("%d / %d", length(shard_keys), length(keys)))

if (tiles_on) {
  say("\n-- the wide cell tiles ------------------------------------------------")
  cd <- file.path(dir_out, "cell")
  n_dirs <- length(list.dirs(cd, recursive = FALSE))
  chk(identical(b$tiles$tiles, n_dirs) && identical(b$tiles$files, n_dirs),
      "exactly one data_0.parquet per tile",
      sprintf("%d tiles, %d files", b$tiles$tiles, b$tiles$files))
  chk(tryCatch({app_one_file_per_partition(cd); TRUE}, error = function(e) FALSE),
      "app_one_file_per_partition()")
  chk(tryCatch({app_cell_tile_check(cd, b$tiles$ncol, con); TRUE},
               error = function(e) FALSE),
      sprintf("tile key holds at the release's nc = %d", b$tiles$ncol))
  wrong <- if (b$tiles$ncol == 7200L) 3103L else 7200L
  chk(inherits(tryCatch(app_cell_tile_check(cd, wrong, con), error = function(e) e),
               "error"),
      sprintf("...and FAILS at the other grid's nc = %d", wrong))
  dg <- app_cell_tile_digests(con, cd)
  chk(all(dg$ok), "per-metric digest equals cell_metric's",
      sprintf("%d of %d metrics", sum(dg$ok), nrow(dg)))
}

if (length(fail)) {
  say("\nFAILED:  ", length(fail), " check(s): ", paste(fail, collapse = " | "))
  quit(save = "no", status = 1)
}
say("\nPASS: the ", ver, " bundle builds and every check held.  ", dir_out)
quit(save = "no", status = 0)
