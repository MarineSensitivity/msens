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

# global 0.05-degree grid: 7200 columns x 3600 rows, cell_id = 1-based row-major.
.CELL_GRID_NCOL <- 7200L
# 50 x 50 cells = 2.5 degrees -> 422 non-empty tiles, avg 1.4M rows (p95 5.0M).
# Finer means more files; coarser means bigger scans per cell.
.CELL_TILE_SIDE <- 50L

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
#' @return character SQL expression
#' @examples
#' cell_model_tile_sql("cell_id")
#' @export
#' @concept calc
cell_model_tile_sql <- function(col = "cell_id") {
  n_across <- .CELL_GRID_NCOL %/% .CELL_TILE_SIDE      # tiles per row-band
  glue::glue(
    "((({col}-1)//{.CELL_GRID_NCOL})//{.CELL_TILE_SIDE})*{n_across}",
    " + ((({col}-1)%{.CELL_GRID_NCOL})//{.CELL_TILE_SIDE})")
}

#' Spatial tile ids covering a set of cells
#'
#' R-side twin of [cell_model_tile_sql()]. Use it to add a `tile IN (…)` filter
#' so DuckDB prunes to the relevant partitions — **without it a query reads every
#' partition**, which is the whole problem `cell_model` exists to solve.
#'
#' @param cell_id integer vector of cell ids
#' @return sorted unique integer tile ids
#' @examples
#' cell_model_tiles(c(1080221L, 1080222L))
#' @export
#' @concept calc
cell_model_tiles <- function(cell_id) {
  cell_id  <- as.integer(cell_id)
  n_across <- .CELL_GRID_NCOL %/% .CELL_TILE_SIDE
  row <- (cell_id - 1L) %/% .CELL_GRID_NCOL
  col <- (cell_id - 1L) %%  .CELL_GRID_NCOL
  sort(unique((row %/% .CELL_TILE_SIDE) * n_across + (col %/% .CELL_TILE_SIDE)))
}
