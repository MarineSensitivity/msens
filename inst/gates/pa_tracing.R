#!/usr/bin/env Rscript
#
# GATE: Program-Area tracing — the scoring fix, checked against published data.
#
#   Rscript inst/gates/pa_tracing.R [db] [ver]
#   MSENS_GATE_DB=~/_big/msens/derived/v9/sdm.duckdb MSENS_GATE_VER=v9 Rscript …
#
# Exit codes: 0 every assertion held · 1 an assertion FAILED · 77 SKIPPED (the
# database is not on this machine). 77 rather than 0, so a CI step that forgets to
# mount the data cannot report a pass it never ran.
#
# Four assertions, two green and one deliberately RED:
#
#   (a) `blend = TRUE, denominator = "all"` reproduces EVERY published `zone_metric`
#       of EVERY Program Area to <= 1e-9. This is the claim the whole fix rests on:
#       the published number is Σ(coalesce(val,0)·pct)/Σ(pct) over every zone cell.
#   (b) `denominator = "study_area"` (D7b, what the app actually uses) stays within
#       0.5 per component on every area — clipping to US waters moves a score, and
#       this bounds by how much.
#   (c) The GAA outline traced through `cells_in_polygon_grid()` yields exactly the
#       published `zone_cell` count, so the grid arithmetic and the release's own
#       `exactextractr` cell set agree on a real 63,417-vertex coastline.
#   (d) THE RED SIDE. The OLD formula must still FAIL: > 0.5 on at least 12 of the
#       the release's own recorded number of areas and by its own GEO margin
#       (v9: 12 of 20 and 49.1447; v7: 4 of 20 and 58.9533). If it ever stops
#       failing, either the data changed or the blend quietly stopped being a blend —
#       and a gate that only checks the good path would call that a pass.
#
# Note (b) and the old formula come from ONE query per area: `scores_for_cells()`
# returns `mean_where_present`, which IS the old formula's value.

args <- commandArgs(trailingOnly = TRUE)
db  <- if (length(args) >= 1) args[1] else Sys.getenv("MSENS_GATE_DB",
         "~/_big/msens/derived/v9/sdm.duckdb")
ver <- if (length(args) >= 2) args[2] else Sys.getenv("MSENS_GATE_VER", "v9")
db  <- path.expand(db)

say  <- function(...) cat(..., "\n", sep = "")
skip <- function(...) { say("SKIPPED: ", ...); quit(save = "no", status = 77) }
die  <- function(...) { say("FAILED:  ", ...); quit(save = "no", status = 1) }

if (!file.exists(db))
  skip("no database at ", db, "\n         pass a path, or set MSENS_GATE_DB; ",
       "this gate needs the release's own sdm.duckdb and never downloads one.")

# Load the SOURCE package when this script sits in one. Never `library(msens)`
# first: a different msens may be installed in the system library, and a gate that
# silently tests the installed 0.42.0 instead of the working tree is worse than no
# gate at all.
# find the script's own directory, so the gate works from any working directory
self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
root <- if (!is.na(self))
  normalizePath(file.path(dirname(self), "..", ".."), mustWork = FALSE) else
    normalizePath(".", mustWork = FALSE)
if (file.exists(file.path(root, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  suppressMessages(pkgload::load_all(root, quiet = TRUE))
  say("msens: source tree ", root)
} else {
  suppressMessages(library(msens))
  say("msens: installed ", as.character(utils::packageVersion("msens")))
}
if (!"blend" %in% names(formals(scores_for_cells)))
  die("the msens on this path has no `blend` argument: it predates the scoring fix ",
      "this gate exists to check.")

suppressMessages({library(DBI); library(duckdb)})

con <- DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE)   # READ-ONLY, always
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
say("database: ", db, "   version: ", ver)

vz <- sdm_val_col(con, "zone")
keys <- DBI::dbGetQuery(con, sprintf(
  "SELECT DISTINCT %s AS k FROM zone WHERE fld = 'programarea_key' ORDER BY 1", vz))$k
if (!length(keys)) die("no programarea_key zones in ", db)
say("Program Areas: ", length(keys), "\n")

TOL_BLEND <- 1e-9      # (a) the blend must be the published number, not merely close
TOL_CLIP  <- 0.5       # (b) how far clipping to US waters may move a component
# (d) PER RELEASE. Both numbers are measurements of a release's own coverage
# profile, not properties of the formula, so neither may be borrowed: v9's GEO
# margin is 49.14477253 over 12 of 20 areas, v7's is 58.95341 over 4 of 20 (v7
# scores fewer components, so fewer areas have a partly-covered one). Each floor is
# stated a digit shy of the measurement, because the assertion is "still this badly
# wrong", not "wrong to the last ulp" -- DuckDB's parallel SUM reorders float
# addition between runs and the blend deltas themselves jitter at 1e-11.
# A release with no record still has to fail the 0.5 tolerance somewhere, and says
# so rather than inheriting another release's numbers.
OLD_FLOOR <- list(
  v9 = list(geo = 49.1447, n = 12),
  v7 = list(geo = 58.9533, n =  4))

rows <- lapply(keys, function(k) {
  cells <- cells_in_pra(con, k)
  pub   <- scores_for_pra(con, k)
  d     <- scores_for_cells(con, cells, denominator = "all")
  clip  <- scores_for_cells(con, cells, denominator = "study_area")

  j  <- merge(pub[, c("component", "score")], d, by = "component")
  jc <- merge(pub[, c("component", "score")], clip, by = "component",
              suffixes = c("_pub", "_clip"))
  data.frame(
    pra      = k,
    n_cells  = nrow(cells),
    d_blend  = max(abs(j$score.y - j$score.x)),
    # the OLD formula's value IS mean_where_present, so one query answers both
    d_old    = max(abs(j$mean_where_present - j$score.x)),
    d_clip   = max(abs(jc$score_clip - jc$score_pub)),
    min_cov  = min(d$coverage),
    worst_old = j$component[which.max(abs(j$mean_where_present - j$score.x))],
    stringsAsFactors = FALSE)
})
tab <- do.call(rbind, rows)
tab <- tab[order(-tab$d_old), ]

print(data.frame(
  `Program Area` = tab$pra, `n cells` = tab$n_cells,
  `max|d| blend` = sprintf("%.2e", tab$d_blend),
  `max|d| study_area` = sprintf("%.4f", tab$d_clip),
  `max|d| OLD formula` = sprintf("%.4f", tab$d_old),
  `worst component` = tab$worst_old,
  `min coverage` = sprintf("%.4f", tab$min_cov),
  check.names = FALSE), row.names = FALSE)
cat("\n")

fail <- character()

## (a) ------------------------------------------------------------------------
if (max(tab$d_blend) > TOL_BLEND)
  fail <- c(fail, sprintf(
    "(a) blend = TRUE does not reproduce zone_metric: max |delta| %.3e > %.0e (%s)",
    max(tab$d_blend), TOL_BLEND, tab$pra[which.max(tab$d_blend)]))
say(sprintf("(a) blend = TRUE reproduces every published zone_metric: max |delta| %.2e <= %.0e  %s",
            max(tab$d_blend), TOL_BLEND, if (max(tab$d_blend) <= TOL_BLEND) "OK" else "FAIL"))

## (b) ------------------------------------------------------------------------
if (max(tab$d_clip) > TOL_CLIP)
  fail <- c(fail, sprintf(
    "(b) denominator = 'study_area' moves %s by %.4f > %.2f",
    tab$pra[which.max(tab$d_clip)], max(tab$d_clip), TOL_CLIP))
say(sprintf("(b) denominator = 'study_area' within %.1f everywhere: max %.4f (%s)  %s",
            TOL_CLIP, max(tab$d_clip), tab$pra[which.max(tab$d_clip)],
            if (max(tab$d_clip) <= TOL_CLIP) "OK" else "FAIL"))

## (c) ------------------------------------------------------------------------
# The fixture is a v9 ARTIFACT. v7's Program-Area vintage
# (ply_programareas_2026_v7) has 14,256 zone_cell rows for GAA where v9's has
# 14,238, so asserting it against v7 is wrong by construction -- a different
# polygon, not a different answer. Run it only when the release's own zone.tbl is
# the one the outline was traced from, and SKIP with the reason otherwise.
meta <- tryCatch(jsonlite::fromJSON(
  system.file("gates", "programarea_gaa_source.json", package = "msens"),
  simplifyVector = TRUE), error = function(e) NULL)
fx <- tryCatch(place_fixture("programarea_gaa"), error = function(e) NULL)
this_tbl <- tryCatch(zone_tbl_for(con, "programarea_key", ver),
                     error = function(e) NA_character_)
if (is.null(fx) || is.null(meta)) {
  fail <- c(fail, "(c) the programarea_gaa fixture or its provenance record is missing")
} else if (!identical(this_tbl, meta$source_zone_tbl)) {
  say(sprintf(paste0(
    "(c) SKIPPED: the outline was traced from %s (%s); this release's Program-Area\n",
    "    table is %s, a different vintage -- %s has %s cells for GAA here."),
    meta$source_zone_tbl, meta$source_ver, this_tbl, this_tbl,
    format(tab$n_cells[tab$pra == meta$zone_key])))
} else {
  traced <- nrow(cells_in_polygon_grid(fx$geometry, fx$grid))
  want   <- tab$n_cells[tab$pra == meta$zone_key]
  if (!length(want)) want <- NA_integer_
  if (!isTRUE(traced == want))
    fail <- c(fail, sprintf(
      "(c) the traced GAA outline gives %d cells, the published zone_cell %s",
      traced, format(want)))
  say(sprintf("(c) GAA traced from its outline (%s): %d cells, published zone_cell %s  %s",
              meta$source_zone_tbl, traced, format(want),
              if (isTRUE(traced == want)) "OK" else "FAIL"))
}

## (d) the RED side -----------------------------------------------------------
n_bad <- sum(tab$d_old > TOL_CLIP)
rec        <- OLD_FLOOR[[ver]]
floor_here <- if (is.null(rec)) NA_real_ else rec$geo
n_min      <- if (is.null(rec)) 1L       else rec$n
geo   <- tab$d_old[tab$pra == "GEO"]
if (!length(geo)) geo <- NA_real_
if (!is.na(floor_here)) {
  if (!isTRUE(geo >= floor_here))
    fail <- c(fail, sprintf(
      "(d) the OLD formula no longer fails on GEO: %s < %.4f (the floor recorded for %s)",
      format(round(geo, 4)), floor_here, ver))
  say(sprintf("(d) the OLD formula still FAILS: GEO %s >= %.4f, and %d of %d areas exceed %.1f  %s",
              format(round(geo, 4)), floor_here, n_bad, nrow(tab), TOL_CLIP,
              if (isTRUE(geo >= floor_here) && n_bad >= n_min) "OK" else "FAIL"))
} else {
  # no floor recorded for this release: still require the old formula to be wrong
  # somewhere, but do not borrow another release's measurement
  if (!isTRUE(max(tab$d_old) > TOL_CLIP))
    fail <- c(fail, sprintf(
      "(d) the OLD formula is within %.1f everywhere on %s: nothing distinguishes it",
      TOL_CLIP, ver))
  say(sprintf(paste0(
    "(d) the OLD formula still FAILS: max %.4f (%s) > %.1f on %d of %d areas  %s\n",
    "    (no per-release floor recorded for %s; v9's 49.1447 is not borrowed)"),
    max(tab$d_old), tab$pra[which.max(tab$d_old)], TOL_CLIP, n_bad, nrow(tab),
    if (max(tab$d_old) > TOL_CLIP && n_bad >= n_min) "OK" else "FAIL", ver))
}
if (n_bad < n_min)
  fail <- c(fail, sprintf(
    "(d) the OLD formula exceeds %.1f on only %d of %d areas, expected >= %d for %s",
    TOL_CLIP, n_bad, nrow(tab), n_min, ver))
say("    (on GAA alone the old formula misses by only ",
    sprintf("%.4f", max(tab$d_old[tab$pra == "GAA"], -Inf)),
    " — GAA is >99 % covered, so GAA cannot discriminate; GEO is the area that does)")

if (length(fail)) { cat("\n"); for (f in fail) say("FAILED:  ", f)
  quit(save = "no", status = 1) }
say("\nPASS: all four assertions held on ", nrow(tab), " Program Areas of ", ver, ".")
quit(save = "no", status = 0)
