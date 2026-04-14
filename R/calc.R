#' Cell-ID SpatRaster
#'
#' Return the global cell-id [`terra::SpatRaster`] used to identify
#' cells for `cells_in_polygon()` and related helpers. The raster is
#' shared across versions (not version-specific).
#'
#' @return a [`terra::SpatRaster`] with a `cell_id` layer
#' @importFrom terra rast
#' @importFrom glue glue
#' @export
#' @concept calc
cell_id_raster <- function() {
  dir_data <- switch(
    Sys.info()[["sysname"]],
    "Darwin" = "~/My Drive/projects/msens/data",
    "Linux"  = "/share/data")
  terra::rast(file.path(dir_data, "derived/r_bio-oracle_planarea.tif"))
}

#' Cells intersecting a polygon
#'
#' Given an sf polygon and a cell-id raster, return a tibble of
#' intersecting `cell_id` with `pct_covered` (0-100). The cell raster
#' uses 0-360 longitudes, so the input polygon is transformed and
#' shifted accordingly.
#'
#' @param poly an sf polygon (assumed or transformable to EPSG:4326)
#' @param r_cell_id a single-layer [`terra::SpatRaster`] of integer cell ids
#' @return a tibble with columns `cell_id` (integer) and `pct_covered` (0-100)
#' @importFrom sf st_transform st_shift_longitude
#' @importFrom terra rasterize vect values
#' @importFrom tibble tibble
#' @export
#' @concept calc
cells_in_polygon <- function(poly, r_cell_id) {
  poly_t <- poly |>
    sf::st_transform(4326) |>
    sf::st_shift_longitude() # [-180,180] -> [0,360]
  r_cov <- terra::rasterize(
    terra::vect(poly_t), r_cell_id,
    cover   = TRUE,
    touches = TRUE)
  r_id_vals  <- terra::values(r_cell_id)[, 1]
  r_cov_vals <- terra::values(r_cov)[, 1]
  keep <- !is.na(r_cov_vals) & r_cov_vals > 0 & !is.na(r_id_vals)
  tibble::tibble(
    cell_id     = as.integer(r_id_vals[keep]),
    pct_covered = round(as.numeric(r_cov_vals[keep]) * 100))
}

#' Cells belonging to a Program Area zone
#'
#' Fast lookup of the cells making up a Program Area by reading
#' directly from the `zone` / `zone_cell` tables, avoiding the
#' `terra::rasterize()` cost paid by [cells_in_polygon()]. Returns
#' the same shape (`cell_id`, `pct_covered`) so downstream helpers
#' can consume it interchangeably; `pct_covered` is always 100
#' because `zone_cell` membership is binary.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param pra_key Program Area key (e.g. "CGM")
#' @return tibble(cell_id integer, pct_covered integer = 100L)
#' @importFrom dplyr tbl filter select inner_join collect mutate join_by
#' @export
#' @concept calc
cells_in_pra <- function(con, pra_key) {
  dplyr::tbl(con, "zone") |>
    dplyr::filter(fld == "programarea_key", value == !!pra_key) |>
    dplyr::select(zone_seq) |>
    dplyr::inner_join(
      dplyr::tbl(con, "zone_cell") |> dplyr::select(zone_seq, cell_id),
      by = dplyr::join_by(zone_seq)) |>
    dplyr::select(cell_id) |>
    dplyr::collect() |>
    dplyr::mutate(pct_covered = 100L)
}

#' Precomputed component scores for a Program Area
#'
#' Reads the precomputed Program Area metrics from the `zone_metric`
#' table instead of aggregating across cells. Returns the same shape
#' as [scores_for_cells()] so it's a drop-in replacement for the
#' score / flower-plot pipeline when the area is a Program Area.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param pra_key Program Area key (e.g. "CGM")
#' @param metric_pattern regex to filter `metric.metric_key`
#'   (default: `"_ecoregion_rescaled$"`)
#' @return tibble(metric_key, score, component, even)
#' @importFrom dplyr tbl filter select inner_join collect mutate join_by
#' @importFrom stringr str_detect str_replace
#' @export
#' @concept calc
scores_for_pra <- function(con, pra_key,
                           metric_pattern = "_ecoregion_rescaled$") {
  dplyr::tbl(con, "zone") |>
    dplyr::filter(fld == "programarea_key", value == !!pra_key) |>
    dplyr::select(zone_seq) |>
    dplyr::inner_join(
      dplyr::tbl(con, "zone_metric") |>
        dplyr::select(zone_seq, metric_seq, score = value),
      by = dplyr::join_by(zone_seq)) |>
    dplyr::inner_join(
      dplyr::tbl(con, "metric") |>
        dplyr::filter(stringr::str_detect(metric_key, metric_pattern)),
      by = dplyr::join_by(metric_seq)) |>
    dplyr::select(metric_key, score) |>
    dplyr::collect() |>
    dplyr::mutate(
      component = metric_key |>
        stringr::str_replace("extrisk_", "") |>
        stringr::str_replace("_ecoregion_rescaled", "") |>
        stringr::str_replace("_", " "),
      even = 1) |>
    dplyr::filter(component != "all")
}

#' Aggregate component scores across a set of cells
#'
#' Weighted-mean aggregation of `cell_metric` across a set of cells;
#' returns a flower-plot-ready tibble with columns
#' `metric_key`, `score`, `component`, `even`.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param cells a tibble from [cells_in_polygon()], with columns
#'   `cell_id` and `pct_covered`
#' @param metric_pattern regex to filter `metric.metric_key`
#'   (default: `"_ecoregion_rescaled$"`)
#' @return tibble(metric_key, score, component, even)
#' @importFrom dplyr tbl filter inner_join group_by summarize collect mutate
#' @importFrom dbplyr copy_inline
#' @importFrom stringr str_detect str_replace
#' @export
#' @concept calc
scores_for_cells <- function(con, cells,
                             metric_pattern = "_ecoregion_rescaled$") {
  cells_t <- dbplyr::copy_inline(con, cells)
  dplyr::tbl(con, "metric") |>
    dplyr::filter(stringr::str_detect(metric_key, metric_pattern)) |>
    dplyr::inner_join(dplyr::tbl(con, "cell_metric"), by = "metric_seq") |>
    dplyr::inner_join(cells_t, by = "cell_id") |>
    dplyr::group_by(metric_key) |>
    dplyr::summarize(
      score = sum(value * pct_covered, na.rm = TRUE) /
              sum(pct_covered, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(
      component = metric_key |>
        stringr::str_replace("extrisk_", "") |>
        stringr::str_replace("_ecoregion_rescaled", "") |>
        stringr::str_replace("_", " "),
      even = 1) |>
    dplyr::filter(component != "all")
}

#' Species table aggregated across a set of cells
#'
#' Returns a tibble with one row per species, aggregated across the
#' supplied cell set with `pct_covered` weighting. Column shape matches
#' the inline query in the mapgl app's drawn-polygon path.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param cells a tibble from [cells_in_polygon()]
#' @return tibble
#' @importFrom dplyr tbl filter select mutate inner_join group_by summarize
#'   collect ungroup join_by
#' @importFrom dbplyr copy_inline
#' @export
#' @concept calc
species_for_cells <- function(con, cells) {
  cells_t <- dbplyr::copy_inline(con, cells)
  tbl_taxon <- dplyr::tbl(con, "taxon") |>
    dplyr::filter(is_ok) |>
    dplyr::select(
      sp_cat,
      sp_common     = common_name,
      sp_scientific = scientific_name,
      taxon_id,
      taxon_authority,
      er_code       = extrisk_code,
      er_score,
      is_mmpa,
      is_mbta,
      mdl_seq) |>
    dplyr::mutate(er_score = er_score / 100)
  dplyr::tbl(con, "model_cell") |>
    dplyr::inner_join(cells_t,   by = "cell_id") |>
    dplyr::inner_join(tbl_taxon, by = dplyr::join_by(mdl_seq)) |>
    dplyr::inner_join(
      dplyr::tbl(con, "cell") |> dplyr::select(cell_id, area_km2),
      by = dplyr::join_by(cell_id)) |>
    dplyr::group_by(
      mdl_seq, sp_cat, sp_common, sp_scientific, taxon_id,
      taxon_authority, er_code, er_score, is_mmpa, is_mbta) |>
    dplyr::summarize(
      area_km2 = sum(area_km2 * pct_covered / 100, na.rm = TRUE),
      avg_suit = sum(value * pct_covered, na.rm = TRUE) /
                 sum(pct_covered, na.rm = TRUE) / 100,
      .groups  = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(
      suit_er      = avg_suit * er_score,
      suit_er_area = avg_suit * er_score * area_km2) |>
    dplyr::group_by(sp_cat) |>
    dplyr::mutate(cat_suit_er_area = sum(suit_er_area, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(pct_cat = suit_er_area / cat_suit_er_area)
}

#' Weighted mean of component scores
#'
#' Convenience wrapper returning the weighted mean of the `score`
#' column from [scores_for_cells()], weighted by `even`.
#'
#' @param d_scores tibble from [scores_for_cells()]
#' @return a numeric scalar
#' @export
#' @concept calc
mean_score <- function(d_scores) {
  stats::weighted.mean(d_scores$score, d_scores$even, na.rm = TRUE)
}

# silence R CMD check NOTEs for dbplyr/dplyr non-standard evaluation
utils::globalVariables(c(
  "metric_key", "metric_seq", "cell_id", "pct_covered", "value",
  "component", "even", "is_ok", "common_name", "scientific_name",
  "taxon_id", "taxon_authority", "extrisk_code", "er_score",
  "is_mmpa", "is_mbta", "mdl_seq", "area_km2", "sp_cat",
  "sp_common", "sp_scientific", "er_code", "avg_suit",
  "suit_er_area", "cat_suit_er_area",
  "fld", "zone_seq"))
