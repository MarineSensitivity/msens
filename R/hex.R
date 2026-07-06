# hex.R — H3 hexagon grid helpers (v8 spatial sampling unit, res 7) ----
#
# v8 replaces the 0.05 degree raster cell with the H3 resolution-7 hexagon
# (~5.16 km^2) as the core sampling/scoring/serving unit. H3 res-7 ids are
# 64-bit and exceed R's exact-double range (2^53 ~= 9e15 < ~6e17), so a hex id
# must NEVER round-trip through an R double. These helpers keep ids inside
# DuckDB as UBIGINT and hand them to R only as the H3 *string* form
# (h3_h3_to_string). All indexing / area / membership uses the DuckDB `h3`
# community extension (plus `spatial` for polygon membership) so results match
# the OBIS h3t tile store byte-for-byte.

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

# run a temp-registered lon/lat query on a (maybe-borrowed) connection ----
.with_con <- function(con, f) {
  own <- is.null(con)
  if (own) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }
  f(con)
}

#' H3 cell ids for lon/lat points (as H3 strings)
#'
#' Indexes points to H3 cells at `res` via the DuckDB `h3` extension. Returns
#' the H3 string form so ids are exact in R.
#'
#' @param lon,lat numeric vectors of longitude / latitude (EPSG:4326)
#' @param res H3 resolution (default [HEX_RES])
#' @param con optional DuckDB connection (a temporary one is used if `NULL`)
#' @return character vector of H3 string ids, aligned to the inputs
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
      "SELECT h3_h3_to_string(h3_latlng_to_cell(lat, lon, {res})) AS hex_id
       FROM hid_tmp ORDER BY .i"))$hex_id
  })
}

#' Centroids (lon/lat) for H3 string ids
#'
#' @param hex_id character vector of H3 string ids
#' @param con optional DuckDB connection (a temporary one is used if `NULL`)
#' @return tibble(hex_id, lon, lat)
#' @export
#' @concept hex
#' @importFrom tibble as_tibble
hex_centroids <- function(hex_id, con = NULL) {
  .with_con(con, function(con) {
    .load_hex_ext(con)
    d <- data.frame(.i = seq_along(hex_id), hex_id = as.character(hex_id))
    duckdb::duckdb_register(con, "hc_tmp", d)
    on.exit(duckdb::duckdb_unregister(con, "hc_tmp"), add = TRUE)
    DBI::dbGetQuery(con,
      "SELECT hex_id,
              h3_cell_to_lng(h3_string_to_h3(hex_id)) AS lon,
              h3_cell_to_lat(h3_string_to_h3(hex_id)) AS lat
       FROM hc_tmp ORDER BY .i") |> tibble::as_tibble()
  })
}

#' Build an H3 hex grid from a raster by exploding pixels into child hexes
#'
#' A res-7 hexagon (~5.16 km^2) is *smaller* than a 0.05 degree ocean pixel
#' (~23 km^2 at 40N), so indexing each pixel centroid to a hex would under-sample
#' the grid (leaving gaps). Instead this explodes each non-NA pixel (per
#' `mask_layer`) into the res-`res` hexes whose *center* falls inside the pixel
#' (DuckDB `h3_polygon_wkt_to_cells`), and each hex inherits that pixel's layer
#' values plus `area_km2` (`h3_cell_area`). Because polyfill uses center
#' containment, adjacent pixels partition the hexes with no overlap. Intended for
#' sources at or coarser than the hex resolution (Bio-Oracle, AquaMaps, NCCOS).
#'
#' @param r a [terra::SpatRaster] in EPSG:4326 (layer names are carried through
#'   as the per-hex value columns)
#' @param con a DuckDB connection
#' @param res H3 resolution (default [HEX_RES])
#' @param mask_layer name of the layer whose non-NA pixels define coverage
#'   (default `names(r)[1]`)
#' @param out_tbl optional DuckDB table to (over)write with `hex_id` as UBIGINT;
#'   if `NULL`, a tibble is returned with `hex_id` as the H3 string
#' @return the `out_tbl` name (invisibly) or a tibble
#' @export
#' @concept hex
#' @importFrom glue glue
#' @importFrom tibble as_tibble
hex_grid_from_raster <- function(r, con, res = HEX_RES,
                                 mask_layer = names(r)[1], out_tbl = NULL) {
  stopifnot(inherits(r, "SpatRaster"), mask_layer %in% names(r))
  .load_hex_ext(con)
  lyrs <- names(r)

  # raster -> points (x=lon, y=lat), keep ocean (mask_layer non-NA)
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = FALSE)
  df <- df[!is.na(df[[mask_layer]]), , drop = FALSE]
  names(df)[match(c("x", "y"), names(df))] <- c("lon", "lat")

  duckdb::duckdb_register(con, "hex_pts_tmp", df)
  on.exit(duckdb::duckdb_unregister(con, "hex_pts_tmp"), add = TRUE)

  hx <- terra::res(r)[1] / 2  # half pixel width  (lon)
  hy <- terra::res(r)[2] / 2  # half pixel height (lat)
  lyr_cols <- paste(lyrs, collapse = ", ")

  # each pixel's bbox as a WKT polygon (SQL string arithmetic on lon/lat)
  wkt <- glue::glue(
    "'POLYGON((' || (lon-{hx}) || ' ' || (lat-{hy}) || ', ' ||",
    " (lon+{hx}) || ' ' || (lat-{hy}) || ', ' ||",
    " (lon+{hx}) || ' ' || (lat+{hy}) || ', ' ||",
    " (lon-{hx}) || ' ' || (lat+{hy}) || ', ' ||",
    " (lon-{hx}) || ' ' || (lat-{hy}) || '))'")

  core <- glue::glue("
    WITH ex AS (
      SELECT unnest(h3_polygon_wkt_to_cells({wkt}, {res})) AS hex_id, {lyr_cols}
      FROM hex_pts_tmp)
    SELECT DISTINCT ON (hex_id)
           hex_id, {lyr_cols},
           h3_cell_area(hex_id, 'km^2') AS area_km2
    FROM ex")

  if (is.null(out_tbl)) {
    DBI::dbGetQuery(con, glue::glue(
      "SELECT h3_h3_to_string(hex_id) AS hex_id, * EXCLUDE (hex_id)
       FROM ({core})")) |> tibble::as_tibble()
  } else {
    DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TABLE {out_tbl} AS {core}"))
    invisible(out_tbl)
  }
}

#' Flag hexes whose centroid falls inside a polygon (DuckDB spatial)
#'
#' Adds a BOOLEAN column `col` to a DuckDB hex table (which must have a `hex_id`
#' UBIGINT column), TRUE where the hex centroid is inside `poly`. Used for the
#' `in_usa` / `in_pra` membership flags on the grid, and as the basis for
#' vector-source SDM interpolation.
#'
#' @param con a DuckDB connection
#' @param hex_tbl name of the DuckDB hex table (with `hex_id` UBIGINT)
#' @param poly path to a polygon file readable by DuckDB `ST_Read`
#'   (e.g. a `.gpkg`)
#' @param col name of the boolean membership column to add
#' @return invisibly, `hex_tbl`
#' @export
#' @concept hex
#' @importFrom glue glue
hex_add_membership <- function(con, hex_tbl, poly, col) {
  stopifnot(file.exists(poly))
  .load_hex_ext(con, spatial = TRUE)
  # dissolve the polygon once, then point-in-polygon per hex centroid
  DBI::dbExecute(con, glue::glue(
    "CREATE OR REPLACE TEMP TABLE _poly_tmp AS
       SELECT ST_Union_Agg(geom) AS g FROM ST_Read('{poly}')"))
  DBI::dbExecute(con, glue::glue(
    "ALTER TABLE {hex_tbl} ADD COLUMN IF NOT EXISTS {col} BOOLEAN"))
  DBI::dbExecute(con, glue::glue(
    "UPDATE {hex_tbl} SET {col} = ST_Contains(
       (SELECT g FROM _poly_tmp),
       ST_Point(h3_cell_to_lng(hex_id), h3_cell_to_lat(hex_id)))"))
  invisible(hex_tbl)
}
