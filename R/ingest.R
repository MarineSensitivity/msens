# ingest.R — interpolate/rasterize source models onto the global 0.05° cell grid ----
#
# Reusable helpers that turn a source distribution into `model_cell`-shaped
# (cell_id, value) rows on the v8 global 0.05° grid, masked to ocean via the
# cell-id COG (build_cell_grid.qmd's `cellid_tif`: cell_id where ocean, NA on land;
# it also hands `cell_id` back directly). One helper per native format:
#   - vector ranges  -> cells_from_ranges()  (rasterize polygons; here)
#   - raster SDMs     -> cells_from_raster()  (resample; here)
#   - AquaMaps 0.5°   -> a bilinear-weight JOIN in DuckDB (ingest_aquamaps.qmd),
#     kept in that notebook because it is SQL over am.duckdb + a precomputed w05.

#' Rasterize range/coverage polygons onto the global 0.05° cell grid
#'
#' Turns presence polygons (an IUCN/BOTW/NMFS range, a critical-habitat polygon)
#' into `(cell_id, value)` on the v8 grid, masked to ocean. By default this is v7's
#' behaviour — a cell whose centre falls in the polygon gets `value` (100 =
#' presence). Set `cover = TRUE` for coverage-weighted values (the fraction of each
#' cell covered by the polygon, × `value`), which softens range edges.
#'
#' @param x an `sf` or `SpatVector` of (multi)polygons in EPSG:4326 [-180,180]
#' @param cellid_tif path to the global cell-id COG (`cell_id` where ocean, NA land)
#' @param value presence value for a covered cell (default 100, matching v7 ranges)
#' @param cover logical; `TRUE` = coverage fraction × `value`, `FALSE` (default) =
#'   binary presence by cell-centre (v7-consistent)
#' @param min_value drop cells at or below this (default 0)
#' @return a tibble `(cell_id integer, value double)` for ocean cells the polygons
#'   cover; empty tibble if none
#' @examples
#' \dontrun{
#' r <- msens::cells_from_ranges(ply_sp, cellid_tif)          # binary presence = 100
#' r <- msens::cells_from_ranges(ply_sp, cellid_tif, cover = TRUE)  # coverage-weighted
#' }
#' @export
#' @concept ingest
#' @importFrom tibble tibble
cells_from_ranges <- function(x, cellid_tif, value = 100, cover = FALSE,
                              min_value = 0) {
  stopifnot(file.exists(cellid_tif))
  v <- if (inherits(x, "SpatVector")) x else terra::vect(x)
  stopifnot(terra::geomtype(v) == "polygons", terra::nrow(v) > 0)

  # cell_id where ocean, NA land; empty if the range is outside the grid extent
  r_cell <- tryCatch(terra::crop(terra::rast(cellid_tif), terra::ext(v)),
                     error = function(e) NULL)
  if (is.null(r_cell) || terra::ncell(r_cell) == 0)
    return(tibble::tibble(cell_id = integer(), value = double()))

  r_val <- if (cover) {
    terra::rasterize(v, r_cell, cover = TRUE) * value             # fraction of cell covered
  } else {
    terra::rasterize(v, r_cell, field = 1) * value                # centre-in-polygon presence
  }
  r_val[r_val <= min_value] <- NA

  s <- c(r_val, r_cell); names(s) <- c("value", "cell_id")
  d <- terra::as.data.frame(s, na.rm = TRUE)                      # ocean-masked (r_cell NA on land)
  tibble::tibble(cell_id = as.integer(d$cell_id), value = round(d$value, 2))
}

#' Resample a raster SDM onto the global 0.05° cell grid
#'
#' For native raster SDMs on a *different* grid than the v8 0.05° (e.g. NCCOS
#' density COGs). Zero-fills absent cells within the source extent then bilinear-
#' resamples onto the cell grid (the same fade as AquaMaps), masks to ocean, drops
#' `< min_value`, and returns `(cell_id, value)`. A source already on the v8
#' topology (AquaX, Bio-Oracle) needs no resample — read it against `cellid_tif`
#' directly.
#'
#' @param r a `SpatRaster` (single layer) in EPSG:4326
#' @param cellid_tif path to the global cell-id COG
#' @param method resampling method (default "bilinear"; use "near" for categorical)
#' @param min_value drop resampled cells below this (default 1)
#' @param zero_fill zero-fill NA within the source extent before resampling so the
#'   surface fades to 0 at its edge (default TRUE)
#' @return a tibble `(cell_id integer, value double)`
#' @export
#' @concept ingest
#' @importFrom tibble tibble
cells_from_raster <- function(r, cellid_tif, method = "bilinear",
                              min_value = 1, zero_fill = TRUE) {
  stopifnot(file.exists(cellid_tif), inherits(r, "SpatRaster"))
  if (zero_fill) r[is.na(r)] <- 0
  r_cell <- terra::crop(terra::rast(cellid_tif), terra::ext(r))
  r_val  <- terra::resample(r, r_cell, method = method)
  r_val[r_val < min_value] <- NA

  s <- c(r_val, r_cell); names(s) <- c("value", "cell_id")
  d <- terra::as.data.frame(s, na.rm = TRUE)
  tibble::tibble(cell_id = as.integer(d$cell_id), value = round(d$value, 2))
}
