# ingest.R — rasterize/resample source models onto the global 0.05° cell grid ----
#
# Reusable helpers that turn a source distribution into `model_cell`-shaped
# (cell_id, value) rows on the v8 GLOBAL 0.05° grid. The grid (build_cell_grid.qmd's
# `cellid_tif`) carries `cell_id = 1:ncell` for EVERY cell (ocean AND land): v8
# captures the **whole** distribution and does NOT mask land — a species' global
# home range matters (birds are largely terrestrial). How marine a range is becomes
# a derived metric (cells_pct_marine()), not a filter. One helper per native format:
#   - vector ranges  -> cells_from_ranges()  (exactextractr coverage; here)
#   - raster SDMs     -> cells_from_raster()  (resample; here)
#   - AquaMaps 0.5°   -> a bilinear-weight JOIN in DuckDB (ingest_aquamaps.qmd),
#     kept there because it is SQL over am.duckdb + a precomputed w05 (ocean cells).

#' Rasterize range polygons onto the global 0.05° cell grid (whole range)
#'
#' Turns presence polygons (an IUCN/BOTW/NMFS range, a critical-habitat polygon)
#' into `(cell_id, value)` on the v8 grid, capturing the **whole** distribution —
#' land AND ocean (v8 does not mask land). Uses `exactextractr`, which is fast and
#' robust to messy geometry (self-intersections, MULTISURFACE — the usual range-map
#' "funk"). By default every cell the polygon overlaps gets `value` (100 =
#' presence); `cover = TRUE` weights by the fraction of each cell covered. Marine
#' share is separate — see [cells_pct_marine()].
#'
#' @param x an `sf`/`SpatVector` of (multi)polygons in EPSG:4326 [-180,180]
#' @param cellid_tif path to the GLOBAL cell-id COG (`cell_id` for every cell)
#' @param value value for a covered cell. For range datasets this is the species'
#'   **extinction-risk score from [compute_er_score()]** (e.g.
#'   `compute_er_score("NMFS:EN")` = 100, `"NMFS:TN"` = 50, `"IUCN:CR"` = 50) —
#'   NEVER a hard-coded number. Default 100 is only a convenience.
#' @param cover logical; `TRUE` = coverage fraction × `value`, `FALSE` (default) =
#'   flat `value` for any overlap
#' @param min_coverage keep cells whose covered fraction exceeds this (default 0 =
#'   any overlap; use 0.5 for a majority/centre-like rule)
#' @param max_vertices (exact path only) subdivide the range into chunks of at
#'   most this many vertices before rasterizing (default 1000). A globe-spanning
#'   range (a wide-ranging seabird) is one massive multipolygon that hangs
#'   `st_union` / `exact_extract`; subdividing bounds each chunk's raster window so
#'   it never touches the whole-globe polygon. Cells are de-duplicated afterward,
#'   so chunking is equivalent to a union here. Needs the `lwgeom` package; without
#'   it, falls back to the single-union path.
#' @param exact force the coverage-aware `exact_extract` path even for the flat
#'   any-overlap case (default `FALSE`). By default the flat case
#'   (`cover=FALSE`, `min_coverage=0`) uses the ~2x-faster `terra::rasterize`
#'   touches path, which yields an identical cell set; set `TRUE` for the
#'   boundary-exact coverage>0 behavior. `cover=TRUE` or `min_coverage>0` always
#'   use the exact path.
#' @return a tibble `(cell_id integer, val double)`; empty if no overlap
#' @examples
#' \dontrun{
#' r <- msens::cells_from_ranges(ply_sp, cellid_tif)               # presence = 100, whole range
#' r <- msens::cells_from_ranges(ply_sp, cellid_tif, cover = TRUE) # coverage-weighted
#' }
#' @export
#' @concept ingest
#' @importFrom tibble tibble
cells_from_ranges <- function(x, cellid_tif, value = 100, cover = FALSE,
                              min_coverage = 0, max_vertices = 1000, exact = FALSE) {
  stopifnot(file.exists(cellid_tif),
            requireNamespace("sf", quietly = TRUE))
  # range polygons are often invalid under s2 (duplicate vertices, self-touch);
  # make-valid with the planar GEOS engine, which tolerates that "funk"
  op <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(op), add = TRUE)
  x <- sf::st_make_valid(sf::st_as_sf(x))

  # fast path (DEFAULT for the flat-value / any-overlap case): terra::rasterize by
  # "touches" — the cells a range overlaps, no per-cell coverage math. ~2x faster than
  # exact_extract on large ranges, with an IDENTICAL cell set (verified, Jaccard 1.0).
  # Falls back to the exact path when coverage weighting is actually needed:
  # `cover=TRUE` (coverage x value), `min_coverage>0` (a fractional threshold), or an
  # explicit `exact=TRUE`.
  if (!cover && min_coverage == 0 && !exact) {
    r  <- terra::rast(cellid_tif)                          # values = cell_id (1:ncell)
    e  <- terra::extract(r, terra::vect(x), touches = TRUE, ID = FALSE)
    ci <- unique(e[[1]]); ci <- ci[!is.na(ci)]
    if (!length(ci)) return(tibble::tibble(cell_id = integer(), val = double()))
    return(tibble::tibble(cell_id = as.integer(ci), val = as.double(value)))
  }

  # exact path: subdivide + exact_extract (coverage-aware)
  stopifnot(requireNamespace("exactextractr", quietly = TRUE))
  geom <- sf::st_combine(sf::st_geometry(x))               # cheap merge, no dissolve

  # subdivide into vertex-bounded chunks so a huge global range doesn't hang the
  # union / whole-globe exact_extract; each chunk reads only its own small window
  chunks <- tryCatch(
    sf::st_sf(geometry = sf::st_collection_extract(
      lwgeom::st_subdivide(geom, max_vertices), "POLYGON")),
    error = function(e) sf::st_sf(geometry = sf::st_union(geom)))

  # exact_extract returns, per covered cell, the raster value (= cell_id) + the
  # fraction of the cell the chunk covers; combine chunks then de-dup cells
  df <- do.call(rbind, exactextractr::exact_extract(
    terra::rast(cellid_tif), chunks, progress = FALSE))
  df <- df[!is.na(df$value) & df$coverage_fraction > min_coverage, , drop = FALSE]
  if (nrow(df) == 0) return(tibble::tibble(cell_id = integer(), value = double()))
  # a cell may fall in >1 chunk -> keep its greatest coverage
  df <- df[order(df$value, -df$coverage_fraction), , drop = FALSE]
  df <- df[!duplicated(df$value), , drop = FALSE]

  v    <- if (cover) round(df$coverage_fraction * value, 2) else as.double(value)
  keep <- v > 0                                            # drop negligible (fp-sliver) cover cells
  tibble::tibble(cell_id = as.integer(df$value[keep]), val = v[keep])   # `val` not `value` (SQL reserved word)
}

#' Percent of a model's cells that are marine (ocean)
#'
#' A derived metric for a range that spans land + ocean: the share of its cells
#' that are ocean. v8 keeps the whole range ([cells_from_ranges()]); this reports
#' how marine it is (e.g. a seabird ~30% marine, a whale ~100%).
#'
#' @param cell_ids integer cell ids of a model's cells
#' @param ocean_cell_ids integer cell ids that are ocean (e.g. the `cell` table's)
#' @param area_km2 optional per-cell areas aligned to `cell_ids` for an
#'   area-weighted percent; `NULL` (default) = cell-count percent
#' @return numeric percent in [0,100], or `NA` if `cell_ids` is empty
#' @export
#' @concept ingest
cells_pct_marine <- function(cell_ids, ocean_cell_ids, area_km2 = NULL) {
  if (length(cell_ids) == 0) return(NA_real_)
  is_ocean <- cell_ids %in% ocean_cell_ids
  if (is.null(area_km2)) round(100 * mean(is_ocean), 1)
  else                   round(100 * sum(area_km2[is_ocean]) / sum(area_km2), 1)
}

#' Resample a raster SDM onto the global 0.05° cell grid
#'
#' For native raster SDMs on a *different* grid than the v8 0.05° (e.g. NCCOS
#' density COGs). Zero-fills absent cells within the source extent then bilinear-
#' resamples onto the cell grid (the same fade as AquaMaps) and returns
#' `(cell_id, value)` for cells at/above `min_value` — the source's own values
#' define coverage (no land mask; a marine SDM simply has no land values). A source
#' already on the v8 topology (AquaX, Bio-Oracle) needs no resample — read it
#' against `cellid_tif` directly.
#'
#' @param r a `SpatRaster` (single layer) in EPSG:4326
#' @param cellid_tif path to the global cell-id COG
#' @param method resampling method (default "bilinear"; use "near" for categorical)
#' @param min_value drop resampled cells below this (default 1)
#' @param zero_fill zero-fill NA within the source extent before resampling so the
#'   surface fades to 0 at its edge (default TRUE)
#' @return a tibble `(cell_id integer, val double)`
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

  s <- c(r_val, r_cell); names(s) <- c("v", "cell_id")
  d <- terra::as.data.frame(s, na.rm = TRUE)
  tibble::tibble(cell_id = as.integer(d$cell_id), val = round(d$v, 2))   # `val` not `value`
}
