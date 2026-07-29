# cell_model.R — the CELL-oriented view of model_cell ----
#
# `serve/model_cell` is partitioned by `mdl_id`: perfect for "give me every cell
# of ONE model" (a titiler tile = one point read), useless for the opposite
# question. Anything per-CELL or per-POLYGON — the scores app's clicked cell, the
# Report tab's arbitrary area — has to scan all ~580M rows, which over S3 fails
# outright (`IO Error: … HTTP GET …/serve/model_cell/`) and even locally is slow.
#
# `serve/cell_model` is the same data partitioned by a 2.5-degree SPATIAL TILE,
# so a cell or a compact polygon touches a handful of small partitions.
#
# The tile formula lives HERE so the writer (release_marine-atlas.qmd) and every
# reader compute it identically — get that wrong and queries silently return
# nothing, because the partition filter excludes the very rows being sought.

# v8's global 0.05-degree grid: 7200 columns x 3600 rows, cell_id = 1-based
# row-major. This is the DEFAULT, not a universal truth — v7 uses a different
# grid entirely (the regional 0-360 bio-oracle raster, 3103 x 2006), so anything
# building or reading a v7 `cell_model` must pass that width. Applying 7200 to
# v7 ids still partitions consistently, so nothing errors; the tiles simply stop
# corresponding to contiguous ground, and a compact polygon scatters across many
# of them — losing the pruning the artifact exists for. See [cell_grid_ncol()].
.CELL_GRID_NCOL <- 7200L
# 50 x 50 cells = 2.5 degrees -> 422 non-empty tiles, avg 1.4M rows (p95 5.0M).
# Finer means more files; coarser means bigger scans per cell.
.CELL_TILE_SIDE <- 50L

#' Grid width (columns) a database's `cell_model` was tiled with
#'
#' The tile key depends on how many columns the grid has, and that differs by
#' version (v8 global: 7200; v7 regional: 3103). Readers must use the SAME width
#' the writer used or `tile IN (…)` prunes away the very rows being sought — and
#' silently, since a wrong tile id is still a valid tile id.
#'
#' Resolution order: the `cell_grid` table written alongside `cell_model`, then
#' the default. Every v8 database predates that table and correctly falls back.
#'
#' @param con a DBI connection, or `NULL` for the default
#' @return integer number of grid columns
#' @importFrom DBI dbGetQuery dbListTables
#' @export
#' @concept calc
cell_grid_ncol <- function(con = NULL) {
  if (is.null(con)) return(.CELL_GRID_NCOL)
  n <- tryCatch({
    if (!"cell_grid" %in% DBI::dbListTables(con)) NULL else
      DBI::dbGetQuery(con, "SELECT ncol FROM cell_grid LIMIT 1")$ncol[1]
  }, error = function(e) NULL)
  if (is.null(n) || is.na(n)) .CELL_GRID_NCOL else as.integer(n)
}

#' SQL expression for a cell's spatial tile id
#'
#' The single definition of the `cell_model` partition key, used by the release
#' notebook when writing and by [cell_model_tiles()] when reading.
#'
#' Note the integer division operator `//`: DuckDB's `/` is FLOAT division, which
#' silently yields a distinct "tile" per cell (and therefore one partition per
#' cell) if used here by mistake.
#'
#' @param col SQL expression giving the cell id, e.g. `"c.cell_id"`
#' @param ncol grid width in columns (default v8's 7200; v7 is 3103 — see
#'   [cell_grid_ncol()])
#' @return character SQL expression
#' @examples
#' cell_model_tile_sql("cell_id")
#' cell_model_tile_sql("cell_id", ncol = 3103)   # v7 grid
#' @export
#' @concept calc
cell_model_tile_sql <- function(col = "cell_id", ncol = .CELL_GRID_NCOL) {
  ncol     <- as.integer(ncol)
  n_across <- ncol %/% .CELL_TILE_SIDE                 # tiles per row-band
  glue::glue(
    "((({col}-1)//{ncol})//{.CELL_TILE_SIDE})*{n_across}",
    " + ((({col}-1)%{ncol})//{.CELL_TILE_SIDE})")
}

#' Spatial tile ids covering a set of cells
#'
#' R-side twin of [cell_model_tile_sql()]. Use it to add a `tile IN (…)` filter
#' so DuckDB prunes to the relevant partitions — **without it a query reads every
#' partition**, which is the whole problem `cell_model` exists to solve.
#'
#' @param cell_id integer vector of cell ids
#' @param ncol grid width in columns; MUST match what the writer used. Pass
#'   `cell_grid_ncol(con)` when reading a database rather than assuming.
#' @return sorted unique integer tile ids
#' @examples
#' cell_model_tiles(c(1080221L, 1080222L))
#' @export
#' @concept calc
cell_model_tiles <- function(cell_id, ncol = .CELL_GRID_NCOL) {
  cell_id  <- as.integer(cell_id)
  ncol     <- as.integer(ncol)
  n_across <- ncol %/% .CELL_TILE_SIDE
  row <- (cell_id - 1L) %/% ncol
  col <- (cell_id - 1L) %%  ncol
  sort(unique((row %/% .CELL_TILE_SIDE) * n_across + (col %/% .CELL_TILE_SIDE)))
}
