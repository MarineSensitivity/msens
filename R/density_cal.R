# Mass-preserving calibration of a density survey folded into the merge (v11 Q6, 2026-10-31).
#
# The v11 attribution showed what an uncalibrated fold-in does: the survey's population percentiles (densest
# cell = 100) replaced AquaMaps' modest suitability -- or, for the NCCOS birds, which have NO AquaMaps model,
# a BirdLife range valued at a constant extinction-risk score -- inside the survey domain, so the surveyed
# species' global share concentrated there (California birds tripled) and every unsurveyed area's quotient
# fell in proportion. The survey knows the species' PATTERN inside its domain far better than a range or a
# global model does; it knows nothing about how much of the species the domain holds relative to the rest
# of the range, and neither does any other source we have. So the calibration preserves the MASS the
# replaced surface gave the domain and lets the survey redistribute it: per taxon,
#
#     f = mean(replaced value over the domain cells that had one) / mean(survey value over the same cells)
#     val_cal = least(100, survey value x f)
#
# where the replaced value is the suitability (AquaMaps / AquaX, max over models) where one existed and the
# taxon's governing er_score (what a range cell carries in the merge) where only a range covered the cell.
# Domain cells with no prior value keep the survey's pattern at the same scale (the fold-in's "domain
# admitted to the footprint"). A taxon with no prior value anywhere in its domain is left at f = 1 and
# flagged. `merge_models.qmd` calls these behind FOLD_DENSITY_CAL=mass; `test-density_cal.R` asserts them.

#' SQL computing the per-taxon mass-preserving factor of a folded-in density survey
#'
#' @param dens character; a relation `(ms_merge_key, cell_id, val)` of the survey values (the annual density
#'   models' rows of the density taxa, after the ingest's 0-100 transform).
#' @param replaced character; a relation `(ms_merge_key, cell_id, val)` of what the survey replaces on those
#'   cells: the suitability (max over AquaMaps / AquaX models) where one existed, else the taxon's governing
#'   er_score where a range dataset covered the cell. Cells absent from it had no prior value.
#' @return SQL selecting `ms_merge_key, n_dom, n_prior, mean_prior, mean_dens, f, calibrated` -- `f` is the
#'   factor (1 when `n_prior = 0`), `calibrated` FALSE for such a taxon.
#' @concept merge
#' @importFrom glue glue
#' @export
density_cal_factors_sql <- function(dens = "dens_vals", replaced = "dens_replaced") {
  stopifnot(is.character(dens), length(dens) == 1L, nzchar(dens),
            is.character(replaced), length(replaced) == 1L, nzchar(replaced))
  glue::glue("
    WITH j AS (
      SELECT d.ms_merge_key, d.cell_id, d.val AS dens, r.val AS prior
      FROM {dens} d LEFT JOIN {replaced} r USING (ms_merge_key, cell_id))
    SELECT ms_merge_key,
           count(*)                                       AS n_dom,
           count(prior)                                   AS n_prior,
           avg(prior)                                     AS mean_prior,
           avg(dens) FILTER (WHERE prior IS NOT NULL)     AS mean_dens,
           CASE WHEN count(prior) > 0 AND avg(dens) FILTER (WHERE prior IS NOT NULL) > 0
                THEN avg(prior) / (avg(dens) FILTER (WHERE prior IS NOT NULL)) ELSE 1.0 END AS f,
           count(prior) > 0                               AS calibrated
    FROM j GROUP BY ms_merge_key", .trim = TRUE)
}

#' SQL applying the mass-preserving factors to a merge-input relation
#'
#' Wraps a SELECT carrying at least `ms_merge_key, ds_key, val` so the rows of the density datasets are
#' scaled by their taxon's factor and capped at 100; every other row, and every other column (`mkey_id`,
#' `cell_id`, ...), passes through unchanged (`m.* REPLACE`). A taxon without a factor row keeps its survey
#' values (f = 1).
#'
#' @param sql character; the merge-input SELECT (`ms_merge_key, ds_key, val` + any other columns).
#' @param dens_ds character vector; the density dataset keys (e.g. `c("gm", "nc", "cce")`).
#' @param factors character; the relation [density_cal_factors_sql()] produced (`ms_merge_key, f`).
#' @return SQL string.
#' @concept merge
#' @importFrom glue glue
#' @export
density_calibrate_sql <- function(sql, dens_ds, factors = "dens_cal") {
  stopifnot(is.character(sql), length(sql) == 1L, nzchar(sql),
            is.character(dens_ds), length(dens_ds) >= 1L, all(nzchar(dens_ds)),
            is.character(factors), length(factors) == 1L, nzchar(factors))
  ds <- paste(sprintf("'%s'", dens_ds), collapse = ", ")
  glue::glue("
    SELECT m.* REPLACE (CASE WHEN m.ds_key IN ({ds}) THEN least(100.0, m.val * coalesce(c.f, 1.0)) ELSE m.val END AS val)
    FROM ({sql}) m LEFT JOIN {factors} c ON c.ms_merge_key = m.ms_merge_key", .trim = TRUE)
}
