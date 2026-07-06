# hex.R — H3 hexagon grid helpers (v8 spatial sampling unit, res 7) ----
#
# v8 replaces the 0.05 degree raster cell with the H3 resolution-7 hexagon
# (~5.16 km^2) as the core sampling/scoring/serving unit.
#
# hex_id is stored as BIGINT (every valid H3 index reserves bit 63, so it fits in
# signed 64-bit) to match the OBIS h3t store (`idx_h3.cell_id`) for cast-free
# joins; R sees it as bit64::integer64 (connections opened bigint="integer64").
# All indexing / area / membership uses the DuckDB `h3` community extension (plus
# `spatial` for polygon membership) so results match the OBIS h3t store exactly.
#
# INTERPOLATION PRINCIPLE — do not regress ------------------------------------
# A value is assigned to a hexagon by INTERPOLATING from the source's centroids
# to the hexagon centroid with a distance-weighted method: hex_interp_idw()
# (inverse-distance weighting over the k nearest source points; for a regular
# grid this is ~bilinear). Two things you must NOT do, because they are
# nearest-neighbour assignment, not interpolation:
#   1. Do NOT give a hex the value of the single source pixel/polygon that merely
#      contains its centre ("inherit").
#   2. Do NOT use h3_polygon_wkt_to_cells to DISTRIBUTE values. That function is
#      used ONLY to ENUMERATE which hexes tile the ocean (hex_ocean); never to
#      interpolate. (Decision logged 2026-07-06 after review — see the
#      feedback_hex_interpolation memory.)

#' H3 resolution of the v8 sampling grid (~5.16 km^2 per hexagon)
#' @export
#' @concept hex
HEX_RES <- 7L

# ensure the DuckDB h3 (and optionally spatial) extension is loaded ----
.load_hex_ext <- function(con, spatial = FALSE) {
  DBI::dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
  if (spatial)
    DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  invisible(con)
}

# run a temp-registered query on a (maybe-borrowed) connection ----
# temp connections use bigint="integer64" so H3 ids (stored as BIGINT) return
# to R exactly as bit64::integer64 rather than a lossy double.
.with_con <- function(con, f) {
  own <- is.null(con)
  if (own) {
    con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }
  f(con)
}

#' H3 cell ids for lon/lat points
#'
#' Indexes points to H3 cells at `res` via the DuckDB `h3` extension. Ids are
#' returned as **BIGINT** (`bit64::integer64` in R) — every valid H3 index
#' reserves bit 63, so it fits in signed 64-bit, and BIGINT matches the OBIS
#' `h3t` store (`idx_h3.cell_id`) for cast-free joins. Use [hex_id_to_string()]
#' for the hex-string form.
#'
#' @param lon,lat numeric vectors of longitude / latitude (EPSG:4326)
#' @param res H3 resolution (default [HEX_RES])
#' @param con optional DuckDB connection (a temporary integer64 one if `NULL`)
#' @return an `integer64` vector of H3 ids, aligned to the inputs
#' @export
#' @concept hex
#' @importFrom glue glue
hex_id_from_lonlat <- function(lon, lat, res = HEX_RES, con = NULL) {
  stopifnot(length(lon) == length(lat))
  .with_con(con, function(con) {
    .load_hex_ext(con)
    d <- data.frame(.i = seq_along(lon), lon = lon, lat = lat)
    duckdb::duckdb_register(con, "hid_tmp", d)
    on.exit(duckdb::duckdb_unregister(con, "hid_tmp"), add = TRUE)
    DBI::dbGetQuery(con, glue::glue(
      "SELECT CAST(h3_latlng_to_cell(lat, lon, {res}) AS BIGINT) AS hex_id
       FROM hid_tmp ORDER BY .i"))$hex_id
  })
}

#' Hex-string form of BIGINT H3 ids (for display)
#'
#' @param hex_id integer64/numeric vector of BIGINT H3 ids
#' @param con optional DuckDB connection (a temporary one is used if `NULL`)
#' @return character vector of H3 strings (e.g. "8729a411cffffff")
#' @export
#' @concept hex
hex_id_to_string <- function(hex_id, con = NULL) {
  .with_con(con, function(con) {
    .load_hex_ext(con)
    d <- data.frame(.i = seq_along(hex_id), hex_id = hex_id)
    duckdb::duckdb_register(con, "hs_tmp", d)
    on.exit(duckdb::duckdb_unregister(con, "hs_tmp"), add = TRUE)
    DBI::dbGetQuery(con,
      "SELECT h3_h3_to_string(CAST(hex_id AS UBIGINT)) AS s FROM hs_tmp ORDER BY .i")$s
  })
}

#' Centroids (lon/lat) for BIGINT H3 ids
#'
#' @param hex_id integer64/numeric vector of BIGINT H3 ids
#' @param con optional DuckDB connection (a temporary integer64 one if `NULL`)
#' @return tibble(hex_id, lon, lat)
#' @export
#' @concept hex
#' @importFrom tibble as_tibble
hex_centroids <- function(hex_id, con = NULL) {
  .with_con(con, function(con) {
    .load_hex_ext(con)
    d <- data.frame(.i = seq_along(hex_id), hex_id = hex_id)
    duckdb::duckdb_register(con, "hc_tmp", d)
    on.exit(duckdb::duckdb_unregister(con, "hc_tmp"), add = TRUE)
    DBI::dbGetQuery(con,
      "SELECT CAST(hex_id AS BIGINT) AS hex_id,
              h3_cell_to_lng(hex_id) AS lon,
              h3_cell_to_lat(hex_id) AS lat
       FROM hc_tmp ORDER BY .i") |> tibble::as_tibble()
  })
}

#' Enumerate the ocean H3 hex tiling from a raster mask (GEOMETRY ONLY)
#'
#' Defines *which* res-`res` hexagons tile the ocean and their `area_km2` — it
#' does **not** interpolate any values (see the module INTERPOLATION PRINCIPLE).
#' A res-7 hex (~5.16 km^2) is smaller than a 0.05 degree pixel (~23 km^2), so
#' each ocean pixel (non-NA in `mask_layer`) is polyfilled into the hexes whose
#' *centre* falls inside it (`h3_polygon_wkt_to_cells`, center-containment → a
#' gap-free, overlap-free tiling). Values are attached afterwards with
#' [hex_interp_idw()] from the source centroids.
#'
#' @param r a [terra::SpatRaster] in EPSG:4326
#' @param con a DuckDB connection
#' @param res H3 resolution (default [HEX_RES])
#' @param mask_layer layer whose non-NA pixels define ocean coverage
#'   (default `names(r)[1]`)
#' @param out_tbl DuckDB table to (over)write with columns `hex_id` (BIGINT) and
#'   `area_km2` (default `"hex"`)
#' @return invisibly, `out_tbl`
#' @export
#' @concept hex
#' @importFrom glue glue
hex_ocean <- function(r, con, res = HEX_RES, mask_layer = names(r)[1],
                      out_tbl = "hex") {
  stopifnot(inherits(r, "SpatRaster"), mask_layer %in% names(r))
  .load_hex_ext(con)
  # ocean pixel centres (mask layer only, drop NA = land)
  df <- terra::as.data.frame(r[[mask_layer]], xy = TRUE, na.rm = TRUE)[, 1:2]
  names(df) <- c("lon", "lat")
  duckdb::duckdb_register(con, "ocean_pts_tmp", df)
  on.exit(duckdb::duckdb_unregister(con, "ocean_pts_tmp"), add = TRUE)

  hx <- terra::res(r)[1] / 2
  hy <- terra::res(r)[2] / 2
  wkt <- glue::glue(
    "'POLYGON((' || (lon-{hx}) || ' ' || (lat-{hy}) || ', ' ||",
    " (lon+{hx}) || ' ' || (lat-{hy}) || ', ' ||",
    " (lon+{hx}) || ' ' || (lat+{hy}) || ', ' ||",
    " (lon-{hx}) || ' ' || (lat+{hy}) || ', ' ||",
    " (lon-{hx}) || ' ' || (lat-{hy}) || '))'")
  DBI::dbExecute(con, glue::glue("
    CREATE OR REPLACE TABLE {out_tbl} AS
    WITH ex AS (
      SELECT unnest(h3_polygon_wkt_to_cells({wkt}, {res})) AS hex_id
      FROM ocean_pts_tmp)
    SELECT DISTINCT ON (hex_id)
           CAST(hex_id AS BIGINT) AS hex_id,
           h3_cell_area(hex_id, 'km^2') AS area_km2
    FROM ex"))
  invisible(out_tbl)
}

#' Interpolate source values onto hex centroids (inverse-distance weighting)
#'
#' The canonical hex interpolation: for every hexagon in `hex_tbl`, take the `k`
#' nearest source points (by great-circle distance, via a unit-sphere kd-tree so
#' it is correct at the poles and antimeridian) and compute the inverse-distance
#' weighted mean (`weight = 1 / d^power`) of each `val_cols` column, skipping NA
#' source values per column. The interpolated columns are appended to `hex_tbl`.
#' For a regular-grid source this reproduces a bilinear-style interpolation; for
#' scattered sources it is classic IDW. This is the ONLY sanctioned way to move
#' values onto hexes — never inherit a containing pixel's value.
#'
#' @param con a DuckDB connection (opened `bigint = "integer64"`)
#' @param src a data.frame of source points with `lon`, `lat`, and `val_cols`
#' @param hex_tbl name of the DuckDB hex table (must have `hex_id` BIGINT)
#' @param val_cols character vector of value columns in `src` to interpolate
#' @param k number of nearest source points (default 8)
#' @param power IDW power (default 2)
#' @param lon,lat coordinate column names in `src` (default "lon","lat")
#' @param chunk hex rows processed per kd-tree query batch (default 5e6)
#' @return invisibly, `hex_tbl`
#' @export
#' @concept hex
#' @importFrom glue glue
hex_interp_idw <- function(con, src, hex_tbl, val_cols, k = 8L, power = 2,
                           lon = "lon", lat = "lat", chunk = 5e6L) {
  stopifnot(is.data.frame(src), all(c(lon, lat, val_cols) %in% names(src)),
            requireNamespace("FNN", quietly = TRUE))
  .load_hex_ext(con)
  rad <- pi / 180
  to_xyz <- function(lo, la) cbind(
    cos(la * rad) * cos(lo * rad),
    cos(la * rad) * sin(lo * rad),
    sin(la * rad))
  src_xyz  <- to_xyz(src[[lon]], src[[lat]])
  src_vals <- as.matrix(src[, val_cols, drop = FALSE])

  # hex centroids (id + lon/lat)
  ids <- DBI::dbGetQuery(con, glue::glue(
    "SELECT hex_id, h3_cell_to_lng(hex_id) AS lon, h3_cell_to_lat(hex_id) AS lat
     FROM {hex_tbl}"))
  DBI::dbExecute(con, "DROP TABLE IF EXISTS _idw_vals")

  n <- nrow(ids)
  first <- TRUE
  for (s in seq(1L, n, by = chunk)) {
    e   <- min(s + chunk - 1L, n)
    knn <- FNN::get.knnx(src_xyz, to_xyz(ids$lon[s:e], ids$lat[s:e]),
                         k = k, algorithm = "kd_tree")
    idx <- knn$nn.index                         # FNN field is nn.index, not nn.idx
    w   <- 1 / (knn$nn.dist ^ power)
    w[!is.finite(w)] <- 1e12                    # exact hit → that neighbour wins
    res <- data.frame(hex_id = ids$hex_id[s:e])
    for (vc in val_cols) {
      vmat <- matrix(src_vals[, vc][idx], nrow = nrow(idx))  # (nq x k) neighbour vals
      wv <- w; na <- is.na(vmat); wv[na] <- 0; vmat[na] <- 0
      den <- rowSums(wv)
      res[[vc]] <- ifelse(den > 0, rowSums(wv * vmat) / den, NA_real_)
    }
    if (first) { DBI::dbWriteTable(con, "_idw_vals", res); first <- FALSE }
    else       { DBI::dbAppendTable(con, "_idw_vals", res) }
  }

  # append interpolated columns to hex_tbl (join on hex_id)
  DBI::dbExecute(con, glue::glue("
    CREATE TABLE {hex_tbl}__i AS
    SELECT h.*, v.* EXCLUDE (hex_id)
    FROM {hex_tbl} h JOIN _idw_vals v USING (hex_id)"))
  DBI::dbExecute(con, glue::glue("DROP TABLE {hex_tbl}"))
  DBI::dbExecute(con, glue::glue("ALTER TABLE {hex_tbl}__i RENAME TO {hex_tbl}"))
  DBI::dbExecute(con, "DROP TABLE IF EXISTS _idw_vals")
  invisible(hex_tbl)
}

#' Flag hexes inside a polygon (H3 polyfill membership)
#'
#' Adds a BOOLEAN column `col` to a DuckDB hex table (with a `hex_id` BIGINT
#' column), TRUE where the hex is inside `poly`. Rather than a point-in-polygon
#' test over every hex (which scales with the grid — prohibitive at ~79M global
#' hexes), this **polyfills** the polygon at `res` to enumerate the in-polygon
#' hexes directly (center containment), then semi-joins. The multipolygon is
#' exploded with `ST_Dump` because `h3_polygon_wkb_to_cells` takes single
#' polygons. Cost is polygon-driven (fixed) rather than grid-driven, so it is the
#' same whether the grid is a region or the whole globe.
#'
#' @param con a DuckDB connection (h3 + spatial loaded on demand)
#' @param hex_tbl name of the DuckDB hex table (with `hex_id` BIGINT)
#' @param poly path to a polygon file readable by DuckDB `ST_Read` (e.g. `.gpkg`)
#' @param col name of the boolean membership column to add
#' @param res H3 resolution the hex grid was built at (default [HEX_RES])
#' @param buffer if `TRUE` (default) buffer the polygon outward by one hex
#'   circumradius before polyfilling, so hexes that *overlap* the boundary (not
#'   only those centred inside) are captured — polyfill is centroid-inclusion, so
#'   without the buffer up to a half-hex-wide fringe along the boundary is missed
#' @return invisibly, `hex_tbl`
#' @export
#' @concept hex
#' @importFrom glue glue
hex_add_membership <- function(con, hex_tbl, poly, col, res = HEX_RES,
                               buffer = TRUE) {
  stopifnot(file.exists(poly))
  .load_hex_ext(con, spatial = TRUE)
  # polyfill includes a hex only if its CENTRE is inside the polygon, so a hex
  # straddling the boundary is dropped. Buffer the polygon outward by one hex
  # circumradius (= half the longest diameter = the avg edge length) so every
  # overlapping hex is captured. Buffer in degrees (km / 111.32); slight
  # over-inclusion at high latitude is harmless (intersected with the ocean grid).
  geom <- if (buffer)
    glue::glue("ST_Buffer(ST_Union_Agg(geom), ",
               "h3_get_hexagon_edge_length_avg({res}, 'km') / 111.32)")
  else "ST_Union_Agg(geom)"
  DBI::dbExecute(con, glue::glue("
    CREATE OR REPLACE TEMP TABLE _in_poly AS
    WITH parts AS (
      SELECT UNNEST(ST_Dump({geom})).geom AS g FROM ST_Read('{poly}'))
    SELECT DISTINCT CAST(hex_id AS BIGINT) AS hex_id FROM (
      SELECT unnest(h3_polygon_wkb_to_cells(ST_AsWKB(g), {res})) AS hex_id FROM parts)"))
  DBI::dbExecute(con, glue::glue(
    "ALTER TABLE {hex_tbl} ADD COLUMN IF NOT EXISTS {col} BOOLEAN DEFAULT FALSE"))
  DBI::dbExecute(con, glue::glue(
    "UPDATE {hex_tbl} SET {col} = TRUE
       WHERE hex_id IN (SELECT hex_id FROM _in_poly)"))
  invisible(hex_tbl)
}
