# Coverage of a zone by a component metric, and the reportability floor it feeds.
#
# A Program-Area component score is a weighted mean over the cells that CARRY the metric, so a
# component modelled in 1.5 % of an area is published on the same footing as one modelled in 99 %.
# v7 already computed that fraction -- `pct_area`, inline in `calc_scores_v7.qmd`'s
# `apply_pctarea_to_programarea_components` chunk -- but only to multiply the score by it, which
# shrinks a sliver toward zero without ever saying it is a sliver. The rule lives here instead of
# in a notebook string so the notebook CALLS it and `test-coverage.R` ASSERTS it.

#' SQL for the coverage of each zone by each component metric
#'
#' Builds the SQL that answers, per (zone, metric): what share of the zone is covered by cells
#' that actually carry that metric? Numerator = the weight of the zone's cells holding a non-NULL
#' value for the metric; denominator = the weight of ALL of that zone's cells. Reads the v1-v7
#' scoring schema (`zone`, `zone_cell`, `cell_metric`, `metric`, and `cell` for `weight = "area"`).
#'
#' `weight = "pct_covered"` reproduces v7's `pct_area` exactly (`SUM(zone_cell.pct_covered)` over
#' each side, as `calc_scores_v7.qmd:2056-2091` computes it), so the same function serves both the
#' reporting floor and the existing weighting. `weight = "area"` (what v10 wants) weights each cell
#' by its **in-zone area**, `cell.area_km2 * pct_covered / 100`: an edge cell half inside the zone
#' contributes half its area, and a 30.8 km2 equatorial cell outweighs a 4.0 km2 Arctic one. On the
#' equal-area-per-cell fixtures of the tests the two agree; on the real grid they do not, which is
#' the point.
#'
#' **A zone with no scored cell yields NO ROW** -- not a zero. That distinction is the whole
#' contract downstream: a component absent from a Program Area is excluded from the composite mean
#' (which is `SUM(value)/COUNT(value)` over present rows), while a zero would drag it down.
#'
#' @param zone_tbl character; the value of `zone.tbl` selecting the zone set, e.g.
#'   `"ply_programareas_2026_v7"`. Only zones of that table are considered.
#' @param metric_keys character vector of `metric.metric_key` values, e.g. the component keys
#'   `extrisk_{sp_cat}_ecoregion_rescaled` plus `primprod_ecoregion_rescaled`.
#' @param weight one of `"pct_covered"` (v7-faithful) or `"area"` (in-zone km2).
#' @param val_col name of the measurement column of `cell_metric`. Defaults to `"value"`, which is
#'   every release v1-v7 (and every served release); a caller holding an open connection should
#'   pass `sdm_val_col(con, "cell_metric")` rather than hardcoding either spelling -- this is a
#'   SQL-string builder and has no connection of its own to introspect.
#' @return SQL string selecting `(zone_seq, metric_key, coverage)`, `coverage` in `[0, 1]`,
#'   ordered by `zone_seq, metric_key`.
#' @examples
#' \dontrun{
#' con <- duckdb::dbConnect(duckdb::duckdb(), sdm_db_path("v7"), read_only = TRUE)
#' d   <- DBI::dbGetQuery(con, coverage_sql(
#'   "ply_programareas_2026_v7",
#'   c(paste0("extrisk_", sp_cats, "_ecoregion_rescaled"), "primprod_ecoregion_rescaled"),
#'   val_col = sdm_val_col(con, "cell_metric")))
#' coverage_floor(d, kappa = 0.05)
#' }
#' @concept calc
#' @importFrom glue glue
#' @export
coverage_sql <- function(
  zone_tbl,
  metric_keys,
  weight  = c("pct_covered", "area"),
  val_col = "value") {

  weight <- match.arg(weight)
  stopifnot(
    is.character(zone_tbl), length(zone_tbl) == 1L, !is.na(zone_tbl),
    is.character(metric_keys), length(metric_keys) > 0L, !any(is.na(metric_keys)),
    is.character(val_col), length(val_col) == 1L, !is.na(val_col))

  zone_tbl_sql <- .sql_str(zone_tbl)
  keys_sql     <- paste(vapply(metric_keys, .sql_str, ""), collapse = ", ")

  # the per-cell weight, and the join it needs. `area` multiplies the cell's area by the fraction
  # of it inside the zone, so an edge cell counts by the area it actually contributes.
  w_expr    <- switch(weight,
    pct_covered = "zc.pct_covered",
    area        = "c.area_km2 * zc.pct_covered / 100.0")
  cell_join <- if (weight == "area") "\n    JOIN cell c ON c.cell_id = zc.cell_id" else ""

  glue::glue("
    WITH
    z AS (
      SELECT zone_seq FROM zone WHERE tbl = {zone_tbl_sql}
    ),
    m AS (
      SELECT metric_seq, metric_key FROM metric WHERE metric_key IN ({keys_sql})
    ),
    zone_total AS (
      SELECT zc.zone_seq, SUM({w_expr}) AS total_weight
      FROM zone_cell zc{cell_join}
      WHERE zc.zone_seq IN (SELECT zone_seq FROM z)
      GROUP BY zc.zone_seq
    ),
    metric_covered AS (
      SELECT zc.zone_seq, cm.metric_seq, SUM({w_expr}) AS metric_weight
      FROM zone_cell zc
      JOIN cell_metric cm ON cm.cell_id = zc.cell_id{cell_join}
      WHERE zc.zone_seq IN (SELECT zone_seq FROM z)
        AND cm.metric_seq IN (SELECT metric_seq FROM m)
        AND cm.{val_col} IS NOT NULL
      GROUP BY zc.zone_seq, cm.metric_seq
    )
    SELECT
      mc.zone_seq,
      m.metric_key,
      CAST(mc.metric_weight AS DOUBLE) / zt.total_weight AS coverage
    FROM metric_covered mc
    JOIN zone_total zt ON zt.zone_seq = mc.zone_seq
    JOIN m            ON m.metric_seq = mc.metric_seq
    ORDER BY mc.zone_seq, m.metric_key",
    .trim = TRUE)
}

#' Flag which (zone, metric) pairs clear the coverage floor
#'
#' Adds `reportable = coverage >= kappa` to the result of [coverage_sql()]. A pair below the floor
#' is **not reportable**: the caller deletes its zone-metric row, so it is excluded from the
#' composite exactly as a component with no cells is -- it is never published as a zero.
#'
#' The boundary is inclusive and is compared with a tolerance, because the fraction arrives from a
#' floating-point division: 5 of 100 equally weighted cells must be reportable at `kappa = 0.05`,
#' and whether `5/100` lands a half-ulp below `0.05` depends on the weights that produced it.
#'
#' @param d data frame with a numeric `coverage` column (typically from [coverage_sql()]).
#' @param kappa numeric in `[0, 1]`; the floor, default `0.05` (5 %).
#' @param tol numeric; absolute tolerance on the boundary comparison, default `1e-9`.
#' @return `d` with a logical `reportable` column added.
#' @examples
#' coverage_floor(data.frame(zone_seq = 1:2, metric_key = "m", coverage = c(0.04, 0.05)))
#' @concept calc
#' @export
coverage_floor <- function(d, kappa = 0.05, tol = 1e-9) {
  stopifnot(
    is.data.frame(d), "coverage" %in% names(d), is.numeric(d$coverage),
    is.numeric(kappa), length(kappa) == 1L, !is.na(kappa), kappa >= 0, kappa <= 1,
    is.numeric(tol), length(tol) == 1L, !is.na(tol), tol >= 0)

  d$reportable <- !is.na(d$coverage) & (d$coverage - kappa) >= -tol
  d
}

# single-quoted SQL literal, doubling any embedded quote
.sql_str <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
