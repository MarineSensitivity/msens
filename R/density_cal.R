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

#' SQL building the "replaced surface" a folded-in density survey is calibrated against
#'
#' One row per (taxon, domain cell) that had a prior value in the merge input: what the control merge gave
#' that cell before the survey replaced it. Two readings of "what it gave":
#' * `mode = "value"`: the control's merged VALUE, `max(suitability, er_score)` -- a range row counts at the
#'   taxon's governing `er_score`, so an Endangered species' prior is ~100 wherever a range covers it and the
#'   calibrated survey saturates at the cap (the pattern is lost again where ER is high).
#' * `mode = "suit"`: the suitability where any suitability dataset models the cell, the `er_score` only where
#'   nothing does (what `dps_sql()` gives an er_mode = 'cell' taxon, which is how a density taxon merges under
#'   FOLD_DENSITY) -- the ER rides beside the value, so the survey pattern survives for high-ER species.
#' Density rows themselves are excluded; the survey domain is `dens_dom` (`ms_merge_key, cell_id`).
#'
#' @param src character; the merge-input relation or SQL source (`mdl_key, cell_id, val`), e.g.
#'   `"read_parquet('.../dist/*/*.parquet')"`.
#' @param prior_ds character vector; the suitability dataset keys (`am`, `ax`, `og`).
#' @param dens_ds character vector; the density dataset keys to exclude.
#' @param mode `"value"` or `"suit"` (see above).
#' @param dens_dom,taxon_model,taxon character; relation names (`ms_merge_key, cell_id`; `mdl_key,
#'   ms_merge_key`; `ms_merge_key, er_score`).
#' @return SQL selecting `ms_merge_key, cell_id, val`.
#' @concept merge
#' @importFrom glue glue
#' @export
density_prior_sql <- function(src, prior_ds, dens_ds, mode = c("value", "suit"),
                              dens_dom = "dens_dom", taxon_model = "taxon_model", taxon = "taxon") {
  mode <- match.arg(mode)
  stopifnot(is.character(src), length(src) == 1L, nzchar(src),
            is.character(prior_ds), length(prior_ds) >= 1L, is.character(dens_ds), length(dens_ds) >= 1L)
  q <- function(x) paste(sprintf("'%s'", x), collapse = ", ")
  rows <- glue::glue("
      SELECT tm.ms_merge_key, mc.cell_id,
             CASE WHEN split_part(mc.mdl_key, '|', 1) IN ({q(prior_ds)}) THEN mc.val END AS suit,
             t.er_score::DOUBLE AS er
      FROM {src} mc JOIN {taxon_model} tm USING (mdl_key)
      JOIN {dens_dom} d ON d.ms_merge_key = tm.ms_merge_key AND d.cell_id = mc.cell_id
      JOIN {taxon} t ON t.ms_merge_key = tm.ms_merge_key
      WHERE split_part(mc.mdl_key, '|', 1) NOT IN ({q(dens_ds)})")
  val <- if (mode == "value") "max(coalesce(suit, er))" else "coalesce(max(suit), max(er))"   # value: a suitability row counts at its value, a range row at er
  glue::glue("SELECT ms_merge_key, cell_id, {val} AS val FROM ({rows}) GROUP BY 1, 2", .trim = TRUE)
}
