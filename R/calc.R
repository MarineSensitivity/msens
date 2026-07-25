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

# Resolve the v7 vs v8 column names for the species-table query (internal).
#
# The v8 rewrite renamed three things this query depends on, and the app-side
# copy of it was never migrated — which is why the v8 "Table of Species" tab came
# up empty with "Can't select columns that don't exist":
#
#   concept          v7                 v8
#   taxon validity   is_ok              is_valid_usa
#   model id         mdl_seq            ms_merge_key (taxon) / mdl_key (model_cell)
#   cell value       value              val           (`value` is reserved in DuckDB)
#
# Resolved per connection so one implementation serves both schemas.
.sdm_cols <- function(con) {
  taxon_cols <- DBI::dbListFields(con, "taxon")
  mc_cols    <- DBI::dbListFields(con, "model_cell")
  pick <- function(cands, have, what) {
    hit <- cands[cands %in% have]
    if (length(hit) == 0)
      stop("cannot resolve the ", what, " column; looked for: ",
           paste(cands, collapse = ", "), call. = FALSE)
    hit[1]
  }
  list(
    valid = pick(c("is_ok", "is_valid_usa"), taxon_cols, "taxon validity"),
    # v7's `is_ok` already baked in the marine/category cull; v8's
    # `is_valid_usa` only means "has >=1 merged cell in US waters", so the
    # scoring-eligibility rules must be applied explicitly (see below).
    marine = if ("is_marine" %in% taxon_cols) "is_marine" else NA_character_,
    tkey  = pick(c("mdl_seq", "ms_merge_key"), taxon_cols, "taxon model-id"),
    mkey  = pick(c("mdl_seq", "mdl_key"), mc_cols, "model_cell model-id"),
    val   = pick(c("value", "val"), mc_cols, "model_cell value"))
}

# The one species-table aggregation, given SQL that yields (cell_id, pct_covered).
# Weighted by pct_covered so partially covered edge cells count proportionally.
.species_sql <- function(con, cells_sql) {
  k <- .sdm_cols(con)
  # SCORING ELIGIBILITY, not just "has cells". v7 encoded this in `is_ok`; v8
  # splits it out, so without these the table lists non-marine and excluded
  # taxa — the v8 run surfaced a cane toad (amphibian) as the first row of the
  # study-area species table.
  marine_clause <- if (is.na(k$marine)) "" else glue::glue(" AND t.{k$marine}")
  glue::glue("
    WITH z AS ({cells_sql})
    SELECT t.sp_cat,
           t.common_name              AS sp_common,
           t.scientific_name          AS sp_scientific,
           t.taxon_id,
           t.taxon_authority,
           t.extrisk_code             AS er_code,
           t.er_score / 100.0         AS er_score,
           t.is_mmpa,
           t.is_mbta,
           CAST(mc.{k$mkey} AS VARCHAR) AS mdl_key,
           sum(c.area_km2 * z.pct_covered / 100.0)                         AS area_km2,
           sum(mc.{k$val} * z.pct_covered) / sum(z.pct_covered) / 100.0    AS avg_suit
    FROM model_cell mc
    JOIN z      USING (cell_id)
    JOIN cell c USING (cell_id)
    JOIN taxon t ON t.{k$tkey} = mc.{k$mkey}
    WHERE t.{k$valid}{marine_clause}
      AND t.sp_cat NOT IN ('reptile', 'amphibian')
    GROUP BY 1,2,3,4,5,6,7,8,9,10")
}

# shared post-aggregation: per-species and per-category contribution shares
.species_shares <- function(d) {
  d |>
    dplyr::mutate(
      suit_er      = .data$avg_suit * .data$er_score,
      suit_er_area = .data$avg_suit * .data$er_score * .data$area_km2) |>
    dplyr::group_by(.data$sp_cat) |>
    dplyr::mutate(cat_suit_er_area = sum(.data$suit_er_area, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(pct_cat = .data$suit_er_area / .data$cat_suit_er_area) |>
    dplyr::arrange(.data$sp_cat, .data$sp_scientific)
}

#' Species table aggregated across a set of cells
#'
#' Returns a tibble with one row per species, aggregated across the supplied
#' cell set with `pct_covered` weighting. Works against **both** the v7 and v8
#' schemas — the differing column names (`is_ok`/`is_valid_usa`,
#' `mdl_seq`/`ms_merge_key`, `value`/`val`) are resolved per connection.
#'
#' Use [species_for_zone()] instead for a whole zone (subregion / Program Area /
#' ecoregion): it resolves the zone's cells inside the database rather than
#' shipping hundreds of thousands of cell ids into the query.
#'
#' @param con a DBI connection to an `sdm.duckdb`
#' @param cells a tibble with `cell_id` and `pct_covered`, e.g. from
#'   [cells_in_polygon()]
#' @return tibble, one row per species; `mdl_key` is the model id as character
#'   (the v7 `mdl_seq` or the v8 `mdl_key`)
#' @export
#' @concept calc
species_for_cells <- function(con, cells) {
  stopifnot(all(c("cell_id", "pct_covered") %in% names(cells)))
  vals <- paste(
    sprintf("(%d, %s)", as.integer(cells$cell_id), as.numeric(cells$pct_covered)),
    collapse = ", ")
  cells_sql <- glue::glue("SELECT * FROM (VALUES {vals}) AS v(cell_id, pct_covered)")
  DBI::dbGetQuery(con, .species_sql(con, cells_sql)) |>
    dplyr::as_tibble() |>
    .species_shares()
}

#' Species table aggregated across a zone
#'
#' One row per species within a named zone (`subregion_key`, `programarea_key`
#' or `ecoregion_key`), aggregated with `pct_covered` weighting.
#'
#' Computed live from `zone_cell` + `model_cell` + `taxon` rather than read from
#' a precomputed table: v7 shipped a `zone_taxon` table, but **v8 does not build
#' one**, which left the app's species table broken. Measured on the v8 database,
#' the largest zone (`subregion_key = "USA"`, ~349k cells, ~10k species) takes
#' ~5 s, so precomputation is not required.
#'
#' @param con a DBI connection to an `sdm.duckdb`
#' @param zone_fld zone field, e.g. `"programarea_key"`
#' @param zone_val zone value, e.g. `"GAA"`
#' @return tibble, same shape as [species_for_cells()]
#' @export
#' @concept calc
species_for_zone <- function(con, zone_fld, zone_val) {
  stopifnot(length(zone_fld) == 1L, length(zone_val) == 1L)
  cells_sql <- glue::glue(
    "SELECT zc.cell_id, zc.pct_covered FROM zone_cell zc ",
    "JOIN zone zn USING (zone_seq) ",
    "WHERE zn.fld = {DBI::dbQuoteString(con, zone_fld)} ",
    "AND zn.val = {DBI::dbQuoteString(con, zone_val)}")
  DBI::dbGetQuery(con, .species_sql(con, cells_sql)) |>
    dplyr::as_tibble() |>
    .species_shares()
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
