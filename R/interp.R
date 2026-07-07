# interp.R — interpolate source model surfaces onto the H3 hex grid ----
#
# The v8 SDM ingest pattern: interpolate each source model onto hex centroids by
# inverse-distance weighting (see the hex.R INTERPOLATION PRINCIPLE — never
# inherit a containing pixel), producing (mdl_seq, hex_id, value) rows written to
# partitioned Parquet for the marine-atlas release.
#
# When every model of a dataset shares ONE source grid (e.g. AquaMaps HCAF 0.5
# degree, used by ~10k species), the hex<-source IDW weights are identical across
# models, so precompute them ONCE with hex_grid_weights() and apply them to each
# model with model_hex_from_weights() (one kNN, then a sparse weighted sum per
# model — far cheaper than a per-model kNN).
#
# ZERO-SURROUND — do not regress ----------------------------------------------
# IDW extrapolates: past the data edge it just holds the nearest values, giving
# bleed and speckled drop-out holes. To make a distribution fade cleanly to 0
# just beyond its footprint, the source `src` passed to hex_grid_weights() MUST
# be the FULL grid (ocean AND land / absent cells), NOT only the cells a model
# occupies — the absent cells act as the surrounding zeros. Pair that with
# `radius_km` ~= one source-cell width so distant present cells cannot reach a
# hex. A model then supplies values only for its present cells; absent neighbours
# contribute 0 to the numerator but stay in `w_total`, so the surface decays over
# ~1 cell. This reproduces the v7 "buffer the range by one cell, add zero points,
# IDW with radius = cell width" method (explore_interpolation.qmd `resample_smooth`).
# Verified vs source on the Hawaii edge 2026-07-07. See feedback_hex_interpolation.

#' Canonical global 0.5-degree cell index for lon/lat
#'
#' A stable integer id for a point's cell in the global 0.5 degree grid
#' (`ext -180,180,-90,90`), so HCAF cells and per-species rasters on that grid
#' share one `src_i` key.
#'
#' @param lon,lat numeric vectors (EPSG:4326)
#' @param res grid resolution in degrees (default 0.5)
#' @return integer cell index (1-based, row-major from top-left)
#' @export
#' @concept interp
cell_i_grid <- function(lon, lat, res = 0.5) {
  ncol <- as.integer(round(360 / res))
  col  <- pmin(ncol - 1L, floor((lon + 180) / res))
  row  <- floor((90 - lat) / res)
  as.integer(row * ncol + col + 1L)
}

#' Precompute IDW weights from a shared source grid to hex centroids
#'
#' One kNN (hex centroids -> source points, great-circle via a unit-sphere
#' kd-tree) whose weights are reused by every model on that source grid. Writes
#' `<out_tbl>` (hex_id, src_i, w) with `k` rows per hex, and `<out_tbl>_sum`
#' (hex_id, w_total) = the IDW denominator (sum of w over the k neighbours,
#' constant per hex regardless of model).
#'
#' @param con DuckDB connection (opened `bigint="integer64"`)
#' @param hex_tbl hex table with `hex_id` BIGINT
#' @param src data.frame with `src_i` (integer) + `lon`,`lat` source-grid points
#' @param k,power IDW parameters (default k=8, power=2)
#' @param where optional SQL predicate on `hex_tbl` (e.g. `"in_usa"`) to limit
#'   the hexes weighted (a US-only score layer needs only US hexes)
#' @param radius_km optional great-circle cap (km): drop source neighbours beyond
#'   this distance so distant points cannot extrapolate. Paired with a full source
#'   grid (ocean + absent cells), this reproduces the zero-surround IDW fade — a
#'   hex beyond ~1 cell of any present value has only absent (0) neighbours in
#'   range and decays to 0. `NULL` = keep all k neighbours (default).
#' @param out_tbl base name for the weight tables (default "hex_src_w")
#' @param chunk hex rows per kd-tree batch (default 5e6)
#' @return invisibly, `out_tbl`
#' @export
#' @concept interp
#' @importFrom glue glue
hex_grid_weights <- function(con, hex_tbl, src, k = 8L, power = 2,
                             where = NULL, radius_km = NULL,
                             out_tbl = "hex_src_w", chunk = 5e6L) {
  stopifnot(all(c("src_i", "lon", "lat") %in% names(src)),
            requireNamespace("FNN", quietly = TRUE))
  DBI::dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
  rad <- pi / 180
  to_xyz <- function(lo, la) cbind(
    cos(la * rad) * cos(lo * rad), cos(la * rad) * sin(lo * rad), sin(la * rad))
  src_xyz <- to_xyz(src$lon, src$lat)
  # unit-sphere chord distance corresponding to radius_km (great-circle)
  chord_max <- if (is.null(radius_km)) Inf else 2 * sin((radius_km / 6371) / 2)

  ids <- DBI::dbGetQuery(con, glue::glue(
    "SELECT hex_id, h3_cell_to_lng(hex_id) AS lon, h3_cell_to_lat(hex_id) AS lat
     FROM {hex_tbl}{if (is.null(where)) '' else paste0(' WHERE ', where)}"))
  DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {out_tbl}"))
  DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {out_tbl}_sum"))

  n <- nrow(ids); first <- TRUE
  for (s in seq(1L, n, by = chunk)) {
    e   <- min(s + chunk - 1L, n)
    knn <- FNN::get.knnx(src_xyz, to_xyz(ids$lon[s:e], ids$lat[s:e]),
                         k = k, algorithm = "kd_tree")
    d <- as.vector(knn$nn.dist)
    w <- 1 / (d ^ power); w[!is.finite(w)] <- 1e12
    keep <- d <= chord_max
    long <- data.frame(
      hex_id = rep(ids$hex_id[s:e], times = k)[keep],
      src_i  = as.integer(src$src_i[as.vector(knn$nn.index)])[keep],
      w      = w[keep])
    if (first) { DBI::dbWriteTable(con, out_tbl, long); first <- FALSE }
    else       { DBI::dbAppendTable(con, out_tbl, long) }
  }
  DBI::dbExecute(con, glue::glue(
    "CREATE TABLE {out_tbl}_sum AS
       SELECT hex_id, sum(w) AS w_total FROM {out_tbl} GROUP BY hex_id"))
  invisible(out_tbl)
}

#' Interpolate one model's source values onto hexes with precomputed weights
#'
#' Applies [hex_grid_weights()] output to a model's (sparse) source values:
#' `hex value = sum(w * value over present source neighbours) / w_total`. Absent
#' source cells contribute 0 to the numerator but are still counted in `w_total`,
#' so the interpolated surface decays smoothly to 0 beyond the model's footprint.
#'
#' @param con DuckDB connection
#' @param model_vals data.frame(`src_i`, `value`) — the model's non-absent source
#'   cells only (e.g. a species' occupied HCAF cells with suitability 1-100)
#' @param mdl_seq model id stamped on the output
#' @param threshold drop interpolated hex values below this (default 0)
#' @param w_tbl weight-table base name from [hex_grid_weights()]
#' @return a tibble(mdl_seq, hex_id, value)
#' @export
#' @concept interp
#' @importFrom glue glue
#' @importFrom tibble as_tibble
model_hex_from_weights <- function(con, model_vals, mdl_seq, threshold = 0,
                                   w_tbl = "hex_src_w") {
  stopifnot(all(c("src_i", "value") %in% names(model_vals)))
  DBI::dbWriteTable(con, "_mv_tmp",
    data.frame(src_i = as.integer(model_vals$src_i),
               value = as.numeric(model_vals$value)),
    overwrite = TRUE)
  out <- DBI::dbGetQuery(con, glue::glue("
    SELECT CAST({mdl_seq} AS INTEGER) AS mdl_seq, w.hex_id,
           sum(w.w * m.value) / s.w_total AS value
    FROM {w_tbl} w
    JOIN _mv_tmp m USING (src_i)
    JOIN {w_tbl}_sum s USING (hex_id)
    GROUP BY w.hex_id, s.w_total
    HAVING sum(w.w * m.value) / s.w_total >= {threshold}"))
  DBI::dbExecute(con, "DROP TABLE IF EXISTS _mv_tmp")
  tibble::as_tibble(out)
}
