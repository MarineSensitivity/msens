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

taxonomy_csv <- Sys.getenv("MSENS_TAXONOMY_CSV", "")
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
b  <- app_bundle_build(con, ver, dir_out, manifest = m, cell_tiles = tiles_on,
                       taxonomy_csv = if (nzchar(taxonomy_csv)) taxonomy_csv else NULL)
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

say("\n-- boot$tables describes app/ exactly --------------------------------")
chk(tryCatch({app_tables_match(dir_out, b$boot); TRUE}, error = function(e) FALSE),
    "no object without a digest, no digest without an object",
    paste(names(b$boot$tables), collapse = ", "))

say("\n-- ids are one text on every object ------------------------------------")
pqs <- unique(dirname(list.files(dir_out, "[.]parquet$", recursive = TRUE, full.names = TRUE)))
pqs <- unique(c(list.files(dir_out, "[.]parquet$", full.names = TRUE),
                grep("/cell$", pqs, value = TRUE)))
float_ids <- character()
for (p in pqs) {
  src <- if (dir.exists(p)) sprintf("'%s/*/data_0.parquet'", p) else sprintf("'%s'", p)
  ty <- tryCatch(DBI::dbGetQuery(con, sprintf(
    "SELECT column_name, column_type FROM (DESCRIBE SELECT * FROM read_parquet(%s))", src)),
    error = function(e) NULL)
  if (is.null(ty)) next
  hit <- ty$column_name %in% msens:::.APP_ID_COLS |
         grepl("_id$", ty$column_name)
  bad <- ty[hit & grepl("^(DOUBLE|FLOAT|REAL|DECIMAL)", toupper(ty$column_type)), ]
  if (nrow(bad)) float_ids <- c(float_ids,
    sprintf("%s:%s(%s)", basename(p), bad$column_name, bad$column_type))
}
chk(!length(float_ids), "no id column is floating point in any published Parquet",
    if (length(float_ids)) paste(float_ids, collapse = " ") else
      sprintf("%d objects", length(pqs)))

dot0 <- Filter(function(f) any(grepl('"[-0-9]+[.]0+"', readLines(f, warn = FALSE))),
               list.files(dir_out, "[.]json$", recursive = TRUE, full.names = TRUE))
chk(!length(dot0), "no id string matches \\.0$ in any published JSON",
    if (length(dot0)) basename(dot0[1]) else "")

say("\n-- the methods block ---------------------------------------------------")
mt <- intersect(c("release_method", "methods"), DBI::dbListTables(con))[1]
if (!is.na(mt)) {
  n_src <- DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", mt))$n
  ok <- !is.null(b$boot$methods) && length(b$boot$methods) == n_src &&
        all(vapply(b$boot$methods, function(x)
          setequal(names(x), c("method_key", "val", "description")), TRUE))
  chk(ok, sprintf("`%s` (%d rows) becomes boot$methods with method_key/val/description", mt, n_src),
      if (is.null(b$boot$methods)) "ABSENT" else sprintf("%d rows", length(b$boot$methods)))
  chk(!any(vapply(b$boot$methods, function(x) "value" %in% names(x), TRUE)),
      "no methods row carries `value`")
} else {
  chk(is.null(b$boot$methods) && !("methods" %in% names(b$boot)),
      "no release_method table -> no `methods` key at all",
      if (is.null(b$boot$methods)) "absent" else "PRESENT")
}

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
