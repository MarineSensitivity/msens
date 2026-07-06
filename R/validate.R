# validate.R — v7↔v8 equivalence + hex-grid sanity checks ----
# The v8 migration changes the spatial sampling unit from the 0.05° raster cell
# to the H3 res-7 hexagon. Interpolation onto hexes shifts values somewhat, so
# the guardrail is that Program-Area composite scores stay *nearly equivalent*
# between v7 and v8 on a common input subset. These helpers make that a
# test-driven, continuously-run gate (see workflows/validate_v7_v8.qmd) rather
# than an end-of-project check. The pure core (score_delta / *_summary /
# assert_within_tolerance / rarity_class / mass_conservation) is unit-tested in
# tests/testthat; the DuckDB wrappers apply it to real score tables.

# default composite metric compared across versions (see calc_scores.qmd)
METRIC_SCORE_DEFAULT <- "score_extriskspcat_primprod_ecoregionrescaled_equalweights"

#' Join two versions' scores and compute per-key deltas (pure)
#'
#' Inner-joins two score tables on `key` and returns the paired values plus
#' `delta = value_b - value_a`. Pure (data-frame in, data-frame out) so it can be
#' unit-tested without a database.
#'
#' @param df_a,df_b data frames each with columns `key` and `value`
#' @param key join column name (default `"programarea_key"`)
#' @param value value column name (default `"score"`)
#' @param labels length-2 suffixes for the two versions (default `c("a","b")`)
#' @return a tibble with `key`, `<value>_<labels[1]>`, `<value>_<labels[2]>`,
#'   `delta`, sorted by descending `abs(delta)`
#' @export
#' @concept validate
#' @importFrom dplyr inner_join arrange desc mutate rename all_of
#' @importFrom tibble as_tibble
score_delta <- function(df_a, df_b, key = "programarea_key", value = "score",
                        labels = c("a", "b")) {
  stopifnot(all(c(key, value) %in% names(df_a)),
            all(c(key, value) %in% names(df_b)),
            length(labels) == 2)
  va <- paste0(value, "_", labels[1])
  vb <- paste0(value, "_", labels[2])
  a <- df_a[, c(key, value)]; names(a) <- c(key, va)
  b <- df_b[, c(key, value)]; names(b) <- c(key, vb)
  dplyr::inner_join(a, b, by = key) |>
    dplyr::mutate(delta = .data[[vb]] - .data[[va]]) |>
    dplyr::arrange(dplyr::desc(abs(delta))) |>
    tibble::as_tibble()
}

#' Summary statistics for a delta table (pure)
#'
#' @param d a tibble from [score_delta()]
#' @param delta_col name of the delta column (default `"delta"`)
#' @return a named list: `n`, `mean_abs`, `max_abs`, `rmse`
#' @export
#' @concept validate
score_delta_summary <- function(d, delta_col = "delta") {
  x <- d[[delta_col]]
  x <- x[is.finite(x)]
  list(
    n        = length(x),
    mean_abs = if (length(x)) mean(abs(x)) else NA_real_,
    max_abs  = if (length(x)) max(abs(x))  else NA_real_,
    rmse     = if (length(x)) sqrt(mean(x^2)) else NA_real_)
}

#' Assert a delta table is within tolerance, else error (pure)
#'
#' The equivalence gate: fails (stops) if `mean|Δ|` exceeds `mean_tol` or
#' `max|Δ|` exceeds `max_tol`. Returns the summary invisibly on success so a
#' notebook can report it.
#'
#' @param d a tibble from [score_delta()]
#' @param mean_tol tolerance on mean absolute delta
#' @param max_tol tolerance on max absolute delta
#' @param delta_col name of the delta column (default `"delta"`)
#' @return (invisibly) the [score_delta_summary()] list
#' @export
#' @concept validate
#' @importFrom glue glue
assert_within_tolerance <- function(d, mean_tol, max_tol, delta_col = "delta") {
  s <- score_delta_summary(d, delta_col)
  if (isTRUE(s$mean_abs > mean_tol) || isTRUE(s$max_abs > max_tol)) {
    stop(glue::glue(
      "score equivalence FAILED: mean|d|={round(s$mean_abs, 4)} ",
      "(tol {mean_tol}), max|d|={round(s$max_abs, 4)} (tol {max_tol}) over ",
      "{s$n} zones"), call. = FALSE)
  }
  message(glue::glue(
    "score equivalence OK: mean|d|={round(s$mean_abs, 4)}, ",
    "max|d|={round(s$max_abs, 4)} over {s$n} zones"))
  invisible(s)
}

#' Rarity class from range size (pure)
#'
#' Bins a global range size (km^2) into an ordered rarity class. Defaults are a
#' starting point to tune against the v8 range-size distribution.
#'
#' @param range_km2 numeric vector of range sizes (km^2)
#' @param breaks upper bounds (km^2) between classes (default
#'   `c(1e4, 1e5, 1e6)`)
#' @param labels ordered class labels (length `length(breaks) + 1`)
#' @return an ordered factor of rarity classes
#' @export
#' @concept validate
rarity_class <- function(range_km2,
                         breaks = c(1e4, 1e5, 1e6),
                         labels = c("very_rare", "rare", "common", "widespread")) {
  stopifnot(length(labels) == length(breaks) + 1)
  cut(range_km2, breaks = c(-Inf, breaks, Inf), labels = labels, ordered_result = TRUE)
}

#' Mass-conservation ratio between a source model and its hex interpolation (pure)
#'
#' Compares an integral of the original model against the same integral computed
#' over the interpolated hexes (e.g. Σ density·area). Returns the ratio and
#' whether it is within `tol` of 1.
#'
#' @param total_source scalar integral over the source representation
#' @param total_hex scalar integral over the interpolated hexes
#' @param tol allowed fractional deviation from 1 (default `0.1`)
#' @return a named list: `ratio`, `within` (logical), `tol`
#' @export
#' @concept validate
mass_conservation <- function(total_source, total_hex, tol = 0.1) {
  ratio <- total_hex / total_source
  list(ratio = ratio, within = isTRUE(abs(ratio - 1) <= tol), tol = tol)
}

#' Program-Area composite-score delta between two version databases
#'
#' Reads the Program-Area composite score from two SDM DuckDB connections (e.g.
#' v7 on the cell grid and v8 on the hex grid) and returns the per-Program-Area
#' [score_delta()]. Centralizes the query previously inlined in
#' `workflows/dev/build_v7.R` so `build_v8.R` and `validate_v7_v8.qmd` share it.
#'
#' @param con_a,con_b DBI connections to the two versions' `sdm.duckdb`
#' @param metric_key composite metric key (default [METRIC_SCORE_DEFAULT])
#' @param labels length-2 version labels (default `c("v7","v8")`)
#' @return a tibble from [score_delta()] keyed by `programarea_key`
#' @export
#' @concept validate
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
pra_score_delta <- function(con_a, con_b,
                            metric_key = METRIC_SCORE_DEFAULT,
                            labels = c("v7", "v8")) {
  q <- glue::glue("
    SELECT z.value AS programarea_key, zm.value AS score
    FROM zone z
    JOIN zone_metric zm USING(zone_seq)
    JOIN metric m USING(metric_seq)
    WHERE z.fld = 'programarea_key' AND m.metric_key = '{metric_key}'")
  score_delta(
    DBI::dbGetQuery(con_a, q),
    DBI::dbGetQuery(con_b, q),
    key = "programarea_key", value = "score", labels = labels)
}
