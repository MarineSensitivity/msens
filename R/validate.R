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
  # identical labels collide into one column and surface as an opaque rlang
  # data-pronoun error from the mutate() below, several frames from the cause
  if (identical(labels[1], labels[2]))
    stop(sprintf("`labels` must differ (both are '%s'); they name the two columns compared",
                 labels[1]), call. = FALSE)
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

#' Name of a table's scalar value column (`val` or `value`)
#'
#' The v8 schema renamed the score/key column from `value` to `val` (`value` is a
#' DuckDB reserved word). Detecting it per-connection lets [pra_score_delta()]
#' compare a v7 database (`value`) against a v8 database (`val`) with one query
#' shape. Prefers `val` when both somehow exist.
#'
#' @param con a DBI connection
#' @param tbl table name to inspect
#' @return `"val"` or `"value"`
#' @keywords internal
#' @importFrom DBI dbGetQuery dbQuoteString
.value_col <- function(con, tbl) {
  cols <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", DBI::dbQuoteString(con, tbl)))$name
  if ("val" %in% cols) "val" else "value"
}

#' Program-Area composite-score delta between two version databases
#'
#' Reads the Program-Area composite score from two SDM DuckDB connections (e.g.
#' v7 on the cell grid and v8 on the hex grid) and returns the per-Program-Area
#' [score_delta()]. Centralizes the query previously inlined in
#' `workflows/dev/build_v7.R` so `build_v8.R` and `validate_versions.qmd` share it.
#'
#' Schema-adaptive: the score/key column is `value` in v7 and `val` in v8 (the
#' reserved-word rename), so the column name is resolved per connection via
#' [.value_col()] rather than hard-coded — otherwise a v7↔v8 (or v8↔v8) comparison
#' errors with "Table z does not have a column named value".
#'
#' Pass `zone_set_key` to pin the comparison to ONE spatial unit. Without it the
#' query selects `fld = 'programarea_key'` from each database and trusts that the
#' two releases meant the same polygons. Measured across every published gpkg,
#' they do — every program-area layer from v2 through v8 is one geometry — but
#' that is a fact about the data, not a guarantee, and BOEM's planning units keep
#' changing. Naming the zone set makes the assumption explicit and checkable;
#' databases predating the column (v1–v7) fall back to the `fld` filter.
#'
#' @param con_a,con_b DBI connections to the two versions' `sdm.duckdb`
#' @param metric_key composite metric key (default [METRIC_SCORE_DEFAULT])
#' @param labels length-2 version labels (default `c("v7","v8")`)
#' @param zone_set_key optional `{zone_type}_{YYYY-MM}` to pin the spatial unit;
#'   errors if a connection carries the column but not that value
#' @return a tibble from [score_delta()] keyed by `programarea_key`
#' @export
#' @concept validate
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
pra_score_delta <- function(con_a, con_b,
                            metric_key = METRIC_SCORE_DEFAULT,
                            labels = c("v7", "v8"),
                            zone_set_key = NULL) {
  q <- function(con, lbl) {
    zc <- .value_col(con, "zone")           # programarea_key string
    mc <- .value_col(con, "zone_metric")    # numeric score
    where <- .zone_where(con, "programarea_key", zone_set_key, lbl)
    glue::glue("
      SELECT z.{zc} AS programarea_key, zm.{mc} AS score
      FROM zone z
      JOIN zone_metric zm USING(zone_seq)
      JOIN metric m USING(metric_seq)
      WHERE {where} AND m.metric_key = '{metric_key}'")
  }
  score_delta(
    DBI::dbGetQuery(con_a, q(con_a, labels[1])),
    DBI::dbGetQuery(con_b, q(con_b, labels[2])),
    key = "programarea_key", value = "score", labels = labels)
}

#' WHERE clause selecting one spatial unit's zones (internal)
#'
#' Pins to `zone_set_key` when both asked for and available; a release predating
#' the column keeps the `fld` filter rather than silently comparing nothing, and
#' naming a zone set the release lacks is an error, not an empty join.
#'
#' @param con a DBI connection to an `sdm.duckdb`
#' @param fld zone field, e.g. `"programarea_key"`
#' @param zone_set_key optional `{zone_type}_{YYYY-MM}`
#' @param lbl label used in the error message
#' @return a length-1 glue string for use after `WHERE` (table alias `z`)
#' @keywords internal
.zone_where <- function(con, fld, zone_set_key, lbl) {
  has_zs <- "zone_set_key" %in%
    DBI::dbGetQuery(con, "PRAGMA table_info('zone')")$name
  if (!is.null(zone_set_key) && has_zs) {
    n <- DBI::dbGetQuery(con, glue::glue(
      "SELECT count(*) n FROM zone WHERE zone_set_key = '{zone_set_key}'"))$n
    if (!n) stop(sprintf("%s: no zones for zone_set_key '%s'", lbl, zone_set_key),
                 call. = FALSE)
    glue::glue("z.zone_set_key = '{zone_set_key}'")
  } else glue::glue("z.fld = '{fld}'")
}

#' Every zone-level score of one spatial unit, across all metrics
#'
#' The long-form reader that feeds [zone_score_delta()]: one row per
#' (zone, metric) for the zones of `fld` — Program Areas by default — in one
#' release's `sdm.duckdb`. Schema-adaptive like [pra_score_delta()] (`value` in
#' v1–v7, `val` in v8+) and pinnable to a zone set the same way, so a v6 database
#' (no `zone_set_key` column) and a v9 database can be read with one call each
#' and compared with [zone_score_delta()].
#'
#' @param con a DBI connection to a release's `sdm.duckdb`
#' @param fld zone field to read (default `"programarea_key"`)
#' @param zone_set_key optional `{zone_type}_{YYYY-MM}` to pin the spatial unit;
#'   errors if a connection carries the column but not that value
#' @param label version label for error messages (default `"db"`)
#' @return a tibble of `zone_key`, `metric_key`, `score`
#' @export
#' @concept validate
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @importFrom tibble as_tibble
zone_scores <- function(con, fld = "programarea_key", zone_set_key = NULL, label = "db") {
  zc <- .value_col(con, "zone")
  mc <- .value_col(con, "zone_metric")
  where <- .zone_where(con, fld, zone_set_key, label)
  DBI::dbGetQuery(con, glue::glue("
    SELECT z.{zc} AS zone_key, m.metric_key, zm.{mc} AS score
    FROM zone z
    JOIN zone_metric zm USING(zone_seq)
    JOIN metric m USING(metric_seq)
    WHERE {where}
    ORDER BY zone_key, metric_key")) |>
    tibble::as_tibble()
}

#' Compare per-zone scores between two releases, across every metric
#'
#' [pra_score_delta()] answers "did this bump move the Program-Area composite?"
#' for one metric and one zone type. This answers the documentation question:
#' *what did this release change, per zone, across all components* — so a release
#' that altered values without altering membership (v4b's turtle merge, v5's MMPA
#' fix) has something quantitative to show, instead of an unchanged species count.
#'
#' **Comparability is asserted, not assumed.** Two releases can label a spatial
#' unit identically and mean different things by it, so the caller must pass units
#' that match. The specific trap: every release's `subregion_key` resolves to the
#' same zone-set vintage, while the member keys are `AK, AKL48, L48, USA` in v1,
#' `AK, FULL, GA, PA, USA` in v7 and `AK, AT, GA, PA, USA` in v8 — a vintage check
#' alone would compare `FULL` against `AT` and report a delta for what is really a
#' redefinition. So this compares only zones and metrics present on BOTH sides,
#' and RETURNS what it had to drop rather than quietly dropping it.
#'
#' Metrics move too: `extrisk_reptile` exists only in v1, `extrisk_all` only in
#' v1–v2, and v8 replaces `extrisk_other` with `extrisk_primary_producer`. A
#' metric on one side only is reported, never compared against nothing.
#'
#' @param a,b data frames of `zone_key`, `metric_key`, `score` for the earlier and
#'   later release
#' @param labels length-2 character labels for `a` and `b`
#' @return a list with `by_metric` (one row per shared metric: `n_zones`,
#'   `mean_a`, `mean_b`, `mean_delta`, `mean_abs_delta`, `max_abs_delta`, `cor`),
#'   `zones_only_a`/`zones_only_b`, `metrics_only_a`/`metrics_only_b`, and
#'   `n_zones_shared`
#' @examples
#' a <- data.frame(zone_key = c("GAA","GAB"), metric_key = "x", score = c(10, 20))
#' b <- data.frame(zone_key = c("GAA","GAB"), metric_key = "x", score = c(12, 20))
#' zone_score_delta(a, b)$by_metric
#' @export
#' @concept validate
zone_score_delta <- function(a, b, labels = c("a", "b")) {
  need <- c("zone_key", "metric_key", "score")
  for (nm in c("a", "b")) {
    d <- get(nm)
    if (!is.data.frame(d) || !all(need %in% names(d)))
      stop(sprintf("`%s` must be a data frame with columns %s", nm,
                   paste(need, collapse = ", ")), call. = FALSE)
  }
  a$zone_key <- as.character(a$zone_key); b$zone_key <- as.character(b$zone_key)
  a$metric_key <- as.character(a$metric_key); b$metric_key <- as.character(b$metric_key)

  zones_a <- sort(unique(a$zone_key));   zones_b <- sort(unique(b$zone_key))
  mets_a  <- sort(unique(a$metric_key)); mets_b  <- sort(unique(b$metric_key))
  shared_z <- intersect(zones_a, zones_b)
  shared_m <- intersect(mets_a, mets_b)

  rows <- lapply(shared_m, function(mk) {
    x <- a[a$metric_key == mk & a$zone_key %in% shared_z, ]
    y <- b[b$metric_key == mk & b$zone_key %in% shared_z, ]
    x <- x[!duplicated(x$zone_key), ]; y <- y[!duplicated(y$zone_key), ]
    m <- merge(x[, c("zone_key", "score")], y[, c("zone_key", "score")],
               by = "zone_key", suffixes = c("_a", "_b"))
    if (!nrow(m)) return(NULL)
    d <- as.numeric(m$score_b) - as.numeric(m$score_a)
    data.frame(
      metric_key     = mk,
      n_zones        = nrow(m),
      mean_a         = mean(as.numeric(m$score_a), na.rm = TRUE),
      mean_b         = mean(as.numeric(m$score_b), na.rm = TRUE),
      mean_delta     = mean(d, na.rm = TRUE),
      mean_abs_delta = mean(abs(d), na.rm = TRUE),
      max_abs_delta  = if (all(is.na(d))) NA_real_ else max(abs(d), na.rm = TRUE),
      # a single constant metric on both sides has no correlation to report;
      # stats::cor would warn and return NA, so say NA deliberately
      cor            = if (nrow(m) > 2 &&
                           stats::sd(m$score_a, na.rm = TRUE) > 0 &&
                           stats::sd(m$score_b, na.rm = TRUE) > 0)
                         stats::cor(as.numeric(m$score_a), as.numeric(m$score_b),
                                    use = "complete.obs") else NA_real_,
      stringsAsFactors = FALSE)
  })
  by_metric <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(by_metric)) by_metric <- data.frame()

  list(
    labels          = labels,
    by_metric       = by_metric,
    n_zones_shared  = length(shared_z),
    zones_only_a    = setdiff(zones_a, zones_b),
    zones_only_b    = setdiff(zones_b, zones_a),
    metrics_only_a  = setdiff(mets_a, mets_b),
    metrics_only_b  = setdiff(mets_b, mets_a))
}

#' Which zone fields of a release carry a given metric
#'
#' A release scores some spatial units and merely carries others: v1 has the
#' composite on Planning Areas and ecoregions, v6 on Program Areas only. Read the
#' `fld` values whose zones have `metric_key`, so a comparison can pick the unit
#' each side actually scored instead of assuming Program Areas.
#'
#' @param con a DBI connection to a release's `sdm.duckdb`
#' @param metric_key metric to look for (default [METRIC_SCORE_DEFAULT])
#' @return character vector of `fld` values (e.g. `"programarea_key"`), with the
#'   number of zones as names; empty if no unit carries the metric
#' @export
#' @concept validate
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
zone_scored_flds <- function(con, metric_key = METRIC_SCORE_DEFAULT) {
  d <- DBI::dbGetQuery(con, glue::glue("
    SELECT z.fld, count(DISTINCT z.zone_seq) AS n
    FROM zone z
    JOIN zone_metric zm USING(zone_seq)
    JOIN metric m USING(metric_seq)
    WHERE m.metric_key = '{metric_key}'
    GROUP BY z.fld ORDER BY z.fld"))
  stats::setNames(d$fld, d$n)
}

#' Crosswalk two zone layers by geometric identity
#'
#' Pairs a zone of `a` with a zone of `b` only when the two polygons are the SAME
#' polygon — intersection over union at least `iou_min` — so two releases scored on
#' differently named units (v1's 36 Planning Areas, v2+'s 20 Program Areas) can be
#' compared where, and only where, they measured the same place. Measured on the
#' 2025 Planning Areas vs the 2026 Program Areas: 18 of 20 Program Areas are
#' identical to a Planning Area (IoU 1.0000); the two Gulf of America Program
#' Areas are subsets (IoU 0.59, 0.15) and must NOT be compared, and 15 Planning
#' Areas (Atlantic, Hawai'i, Pacific islands) have no Program Area at all.
#'
#' Candidate pairs come from `hint` when given (a column of `b` naming zones of
#' `a`, e.g. `programarea` rows carry `planarea_key = "CGA,EGA"`), else every pair
#' that intersects; either way the geometry decides. Planar areas in the layer's
#' CRS (`sf::sf_use_s2(FALSE)` while computing).
#'
#' @param a,b `sf` polygon layers
#' @param key_a,key_b name of the zone-key column in each
#' @param iou_min minimum intersection/union to call two zones identical (default
#'   `0.999`)
#' @param hint optional column of `b` listing (comma-separated) `key_a` values to
#'   test against; a hint with more than one key is tested as their union
#' @return a data frame of every candidate pair: `key_a`, `key_b`, `iou`,
#'   `identical` (logical); the compared set is `identical == TRUE`
#' @export
#' @concept validate
zone_crosswalk <- function(a, b, key_a, key_b, iou_min = 0.999, hint = NULL) {
  stopifnot(inherits(a, "sf"), inherits(b, "sf"), key_a %in% names(a), key_b %in% names(b))
  old <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old), add = TRUE)
  a <- sf::st_make_valid(a); b <- sf::st_make_valid(b)
  ga <- sf::st_geometry(a); gb <- sf::st_geometry(b)
  ka <- as.character(a[[key_a]]); kb <- as.character(b[[key_b]])
  iou <- function(g, h) {
    ai <- suppressWarnings(as.numeric(sf::st_area(sf::st_intersection(g, h))))
    au <- suppressWarnings(as.numeric(sf::st_area(sf::st_union(g, h))))
    if (!length(ai) || !is.finite(au) || au == 0) 0 else sum(ai) / au
  }
  rows <- lapply(seq_along(kb), function(j) {
    if (!is.null(hint)) {
      keys <- trimws(strsplit(as.character(b[[hint]][j]), ",")[[1]])
      keys <- keys[keys %in% ka]
      if (!length(keys)) return(NULL)
      data.frame(key_a = paste(keys, collapse = ","), key_b = kb[j],
                 iou = iou(gb[j], sf::st_union(ga[ka %in% keys])), stringsAsFactors = FALSE)
    } else {
      hits <- which(lengths(suppressMessages(sf::st_intersects(ga, gb[j]))) > 0)
      if (!length(hits)) return(NULL)
      data.frame(key_a = ka[hits], key_b = kb[j],
                 iou = vapply(hits, function(i) iou(gb[j], ga[i]), numeric(1)),
                 stringsAsFactors = FALSE)
    }
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) out <- data.frame(key_a = character(), key_b = character(), iou = numeric())
  out$iou <- round(out$iou, 4)
  out$identical <- out$iou >= iou_min
  out[order(-out$iou, out$key_b), ]
}
