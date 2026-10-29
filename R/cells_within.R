# Cells within a distance of a set of source cells, by cell-id arithmetic on a regular
# longitude-latitude grid (v11 Q1: the buffered-record range dataset `rng_occ`, plan 2026-10-29
# §4.3 / page §13d; Q2: a zone dilated by the buffer `d` so a record just outside it counts).
# Pure SQL -- no spatial extension, no raster -- so it runs wherever DuckDB does, and the
# antimeridian needs no special case because the column index wraps. `ingest_obis-occurrence.qmd`
# and `explore_occurrence_rule.qmd` CALL it; `test-cells_within.R` ASSERTS it against a
# brute-force scan of every cell.

#' SQL for the cells within `d_km` of a set of source cells
#'
#' A cell is within `d_km` of a source cell when the great-circle (haversine, R = 6371.0088 km)
#' distance between the two CELL CENTRES is at most `d_km`. For each source cell the candidates are
#' a window of rows (`+- ceiling(d / row height) + 1`) and columns (the exact longitude reach of a
#' `d_km` cap at the source latitude, `asin(sin(d/R) / cos(lat))`; every column when the cap holds
#' a pole), the column index wrapping modulo `nc` (so a cell at 179.975 E reaches the cells at
#' -179.975) and the rows clipped at the poles; the haversine filter then decides. The grid is
#' `cell_id = row * nc + col + 1`, row 0 at `ymax` (90 N), col 0 at `xmin` (-180), `deg` per cell --
#' `global05` by default (`grid_spec_for("global05")`), any regular global grid by argument.
#'
#' Land cells are included (the whole-range convention); the caller masks if it wants to.
#'
#' @param src character; a relation with a `cell_id` column and, optionally, the grouping column
#'   `by` (e.g. `mdl_key`): the result is distinct `(by, cell_id)`.
#' @param d_km numeric; the distance, km (> 0).
#' @param by character or `NULL`; the grouping column of `src` carried through.
#' @param nc,nr integer; the grid's columns and rows (7200 x 3600).
#' @param deg numeric; the cell size in degrees (0.05). `nc * deg` must be 360 and `nr * deg` 180.
#' @return SQL selecting distinct `[by,] cell_id` (INTEGER).
#' @concept range
#' @importFrom glue glue
#' @export
#' @examples
#' cat(cells_within_sql("src_cells", 100))
cells_within_sql <- function(src, d_km, by = NULL, nc = 7200L, nr = 3600L, deg = 0.05) {
  stopifnot(
    is.character(src), length(src) == 1L, nzchar(src),
    is.numeric(d_km), length(d_km) == 1L, is.finite(d_km), d_km > 0,
    is.null(by) || (is.character(by) && length(by) == 1L && nzchar(by)),
    is.numeric(nc), is.numeric(nr), is.numeric(deg),
    "nc * deg must be 360 and nr * deg must be 180 (a regular global grid)" =
      isTRUE(all.equal(nc * deg, 360)) && isTRUE(all.equal(nr * deg, 180)))
  nc <- as.integer(nc); nr <- as.integer(nr)
  r_km   <- 6371.0088
  km_deg <- r_km * pi / 180                           # km per degree of arc
  n_rows <- ceiling(d_km / (km_deg * deg)) + 1L       # rows to either side
  by_sel <- if (is.null(by)) "" else paste0(by, ", ")
  glue::glue("
    WITH s AS (
      SELECT {by_sel}cell_id,
             (CAST(cell_id AS BIGINT) - 1) // {nc} AS r0, (CAST(cell_id AS BIGINT) - 1) % {nc} AS c0,
             90 - ((CAST(cell_id AS BIGINT) - 1) // {nc} + 0.5) * {deg}   AS lat0,
             -180 + ((CAST(cell_id AS BIGINT) - 1) % {nc} + 0.5) * {deg}  AS lon0
      FROM {src}),
    w AS (
      SELECT *, CASE WHEN sin({d_km} / {r_km}) / cos(radians(lat0)) >= 1 THEN {nc}
                     ELSE CAST(ceil(degrees(asin(sin({d_km} / {r_km}) / cos(radians(lat0)))) / {deg}) AS INTEGER) + 1 END AS dcm
      FROM s),
    cand AS (
      SELECT {by_sel}lat0, lon0, r0 + dr AS r1, ((c0 + dc) % {nc} + {nc}) % {nc} AS c1
      FROM w, range(-{n_rows}, {n_rows} + 1) a(dr),
              generate_series(-least(dcm, {nc %/% 2L}), least(dcm, {nc %/% 2L})) b(dc)
      WHERE r0 + dr BETWEEN 0 AND {nr - 1L}),
    cand2 AS (
      SELECT {by_sel}lat0, lon0, r1, c1,
             90 - (r1 + 0.5) * {deg} AS lat1, -180 + (c1 + 0.5) * {deg} AS lon1
      FROM cand)
    SELECT DISTINCT {by_sel}CAST(r1 * {nc} + c1 + 1 AS INTEGER) AS cell_id
    FROM cand2
    WHERE 2 * {r_km} * asin(sqrt(least(1,
            pow(sin(radians(lat1 - lat0) / 2), 2) +
            cos(radians(lat0)) * cos(radians(lat1)) * pow(sin(radians(lon1 - lon0) / 2), 2)))) <= {d_km}",
    .trim = TRUE)
}

#' The cell id of a longitude / latitude on a regular global grid
#'
#' @param lon,lat numeric vectors (degrees).
#' @inheritParams cells_within_sql
#' @return integer cell ids (`row * nc + col + 1`, row 0 at 90 N, col 0 at -180); a point at
#'   exactly 180 E or -90 S falls in the last column / row.
#' @concept range
#' @export
cell_id_of <- function(lon, lat, nc = 7200L, nr = 3600L, deg = 0.05) {
  stopifnot(length(lon) == length(lat))
  col <- pmin(floor((lon + 180) / deg), nc - 1)
  row <- pmin(floor((90 - lat) / deg), nr - 1)
  as.integer(row * nc + col + 1)
}
