# range constraint: trim a model surface with the model's own error rates ----
#
# The rule ("E" on the scorecard of workflows/obis_range_constraint.qmd) is three tests, the same
# for every species, with no per-species tuning:
#
#   1. threshold   the value that keeps `sens` (0.90) of HELD-OUT presences  -> sens_threshold(),
#                  heldout_threshold()
#   2. region      a confusion matrix per region, with search effort: among the cells the model
#                  calls present AND somebody searched, are there too few records of the species
#                  for the precision the model shows everywhere else (significantly, and by a
#                  factor: less than `ratio` of that precision)?              -> range_region_sql(),
#                  region_effort_test()
#   3. uncertainty a cell stays only if its value minus one bootstrap spread still clears the
#                  threshold, and it is not an extrapolation                  -> range_constrain_sql()
#
# Critical habitat is never trimmed. A dropped region that holds a record of the species is
# labelled "documented occurrence only" and contributes no cells.
#
# The notebook CALLS these and tests/testthat/test-range.R ASSERTS them, so the two cannot drift.

#' Threshold that keeps a fixed share of presences
#'
#' The largest value `t` such that at least `sens` of the predictions at presences are `>= t`:
#' the confusion-matrix threshold that fixes sensitivity. `sens = 0.9` is the "10th percentile
#' training presence" (P10) when `pred` are training predictions, and the honest version of it
#' when they are held-out predictions. No interpolation: the result is always one of the values
#' in `pred`, which is what OBIS's pipeline does (`rev(sort(x))[ceiling(n * 0.9)]`), so a
#' published P10 is reproduced exactly.
#'
#' @param pred numeric predictions at presence points (missing values are dropped)
#' @param sens share of presences to keep, in (0, 1]
#'
#' @return a single number
#' @export
#' @concept range
#'
#' @examples
#' sens_threshold(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) # 20: nine of ten are >= 20
sens_threshold <- function(pred, sens = 0.9) {
  stopifnot(
    is.numeric(pred),
    "`sens` must be one number in (0, 1]" = length(sens) == 1 && is.finite(sens) && sens > 0 && sens <= 1)
  pred <- pred[!is.na(pred)]
  stopifnot("no non-missing prediction at a presence" = length(pred) > 0)
  sort(pred, decreasing = TRUE)[ceiling(length(pred) * sens)]
}

#' Confusion counts at a threshold
#'
#' True and false positives and negatives of `pred >= thr` against `presence`, with the three
#' rates the range rule reports. Against random background a "false positive" is only a
#' background point above the threshold, so `spec` and `precision` describe the model against its
#' background, not against true absence.
#'
#' @param pred numeric predictions
#' @param presence 1 (or `TRUE`) for a presence, 0 for a background / absence point
#' @param thr threshold on the scale of `pred`
#'
#' @return a one-row data frame: `thr, tp, fp, tn, fn, sens, spec, precision`
#' @export
#' @concept range
confusion_at <- function(pred, presence, thr) {
  stopifnot(is.numeric(pred), length(pred) == length(presence), length(thr) == 1, is.finite(thr))
  ok       <- !is.na(pred) & !is.na(presence)
  pred     <- pred[ok]
  presence <- as.logical(presence[ok])
  pos      <- pred >= thr
  tp <- sum(pos & presence);  fp <- sum(pos & !presence)
  fn <- sum(!pos & presence); tn <- sum(!pos & !presence)
  data.frame(
    thr       = thr,
    tp        = tp, fp = fp, tn = tn, fn = fn,
    sens      = if (tp + fn > 0) tp / (tp + fn) else NA_real_,
    spec      = if (tn + fp > 0) tn / (tn + fp) else NA_real_,
    precision = if (tp + fp > 0) tp / (tp + fp) else NA_real_)
}

#' Held-out threshold at a fixed sensitivity
#'
#' The threshold of the range rule's first test. Pooled over the cross-validation folds, it is
#' the value that keeps `sens` of the presences **the fold's model never saw**
#' ([sens_threshold()] on the held-out predictions). Published thresholds are fitted on training
#' data, where a flexible model scores its own presences too high; this one is not.
#'
#' When the predictions of each fold's model on its own training rows are supplied (`cv_train`),
#' the per-fold table is the honest confusion matrix: the threshold is fixed at `sens` on the
#' fold's TRAINING presences and the counts are taken on its TEST rows. Its `sens` column then
#' says how much sensitivity a training threshold really delivers.
#'
#' @param cv data frame of held-out predictions: `fold`, `presence` (1/0), `pred`
#' @param cv_train optional data frame of the same shape: each fold's model on its training rows
#' @param sens sensitivity to fix
#'
#' @return a list: `thr` (the pooled held-out threshold), `pooled` ([confusion_at()] of all
#'   held-out rows at `thr`) and `folds` (one [confusion_at()] row per fold, with `fold` and
#'   `thr_from` = `"train"` or `"pooled"`)
#' @export
#' @concept range
heldout_threshold <- function(cv, cv_train = NULL, sens = 0.9) {
  need <- c("fold", "presence", "pred")
  stopifnot(
    "`cv` needs columns fold, presence, pred" = all(need %in% names(cv)),
    "`cv_train` needs columns fold, presence, pred" = is.null(cv_train) || all(need %in% names(cv_train)))
  thr   <- sens_threshold(cv$pred[cv$presence == 1], sens)
  folds <- lapply(sort(unique(cv$fold)), function(f) {
    te <- cv[cv$fold == f, ]
    if (is.null(cv_train)) {
      thr_f <- thr; from <- "pooled"
    } else {
      tr <- cv_train[cv_train$fold == f, ]
      stopifnot("a fold of `cv` has no rows in `cv_train`" = nrow(tr) > 0)
      thr_f <- sens_threshold(tr$pred[tr$presence == 1], sens); from <- "train"
    }
    cbind(fold = f, thr_from = from, confusion_at(te$pred, te$presence, thr_f))
  })
  list(
    thr    = thr,
    pooled = confusion_at(cv$pred, cv$presence, thr),
    folds  = do.call(rbind, folds))
}

#' The regional test: is the model contradicted where people looked?
#'
#' The range rule's second test, one row per region. Among the units (cells, or blocks of cells)
#' the model calls present, `n_searched` were searched (they hold a record of the species'
#' target group) and `n_occupied` of those hold a record of the species itself. The model's
#' precision everywhere **else** (`p_core`, the same ratio over all other searched units) says
#' how many records to expect; the binomial tail `P(X <= n_occupied | n_searched, p_core)` says
#' how surprising this few are.
#'
#' * `p_value < alpha` **and** the region's own record rate (`n_occupied / n_searched`) is below
#'   `ratio` times `p_core` -> `"dropped"`: searched, and the species is not there.
#' * fewer searched units than `n_min`, the smallest number at which even **zero** records could
#'   reach `alpha` -> `"unsearched"`: kept, and flagged. Empty of observers is not empty of turtles.
#' * otherwise `"kept"`.
#'
#' The second condition is the effect size. With hundreds of searched units the binomial tail
#' alone rejects any region where the species is merely scarcer than in its stronghold: on the
#' published OBIS models it dropped the whole US Pacific for the loggerhead (139 occupied blocks
#' of 978 searched, against 23 % elsewhere) and for the green turtle, Hawaii included. A region
#' is contradicted only when records are both significantly AND materially fewer. `ratio = 1`
#' is the test without the guard.
#'
#' A dropped region with at least one record of the species (`n_records > 0`) is labelled
#' `"documented occurrence only"`: the vagrant is on the record, the region is not range.
#'
#' Leaving the tested region out of `p_core` is what lets the test see a region at all: a species'
#' stronghold is compared with the rest of its range, not with itself.
#'
#' @param n_searched model-present units in the region that were searched
#' @param n_occupied those among them holding a record of the species
#' @param n_searched_all,n_occupied_all the same two counts over ALL model-present units (one
#'   value per species, recycled)
#' @param n_records units in the region holding a record of the species, whether or not the
#'   model calls them present (decides the label only)
#' @param alpha rejection level
#' @param ratio a region is dropped only if its record rate is below this share of `p_core`
#'   (in (0, 1]; 1 = significance alone)
#'
#' @return a data frame, one row per region: `p_core, rate, n_min, p_value, verdict, label`
#' @importFrom stats pbinom
#' @export
#' @concept range
#'
#' @examples
#' # 400 searched units and 2 records, where the rest of the range shows 1 record in 10
#' region_effort_test(n_searched = 400, n_occupied = 2, n_searched_all = 5400, n_occupied_all = 502)
region_effort_test <- function(
    n_searched, n_occupied, n_searched_all, n_occupied_all,
    n_records = n_occupied, alpha = 0.05, ratio = 0.25) {
  stopifnot(
    length(n_searched) == length(n_occupied),
    all(n_occupied <= n_searched), all(n_searched <= n_searched_all), all(n_occupied <= n_occupied_all),
    length(alpha) == 1, alpha > 0, alpha < 1,
    "`ratio` must be one number in (0, 1]" = length(ratio) == 1 && is.finite(ratio) && ratio > 0 && ratio <= 1)
  n_core <- n_searched_all - n_searched
  k_core <- n_occupied_all - n_occupied
  p_core <- ifelse(n_core > 0, k_core / n_core, NA_real_)
  # fewest searched units at which zero records would reject: (1 - p)^n < alpha
  n_min  <- ifelse(
    is.na(p_core) | p_core <= 0, Inf,
    ifelse(p_core >= 1, 1, floor(log(alpha) / log1p(-p_core)) + 1))
  p_value <- ifelse(is.na(p_core), NA_real_, pbinom(n_occupied, n_searched, p_core))
  rate    <- ifelse(n_searched > 0, n_occupied / n_searched, NA_real_)
  verdict <- ifelse(
    n_searched < n_min, "unsearched",
    ifelse(p_value < alpha & rate < ratio * p_core, "dropped", "kept"))
  data.frame(
    p_core  = p_core,
    rate    = rate,
    n_min   = n_min,
    p_value = p_value,
    verdict = verdict,
    label   = ifelse(verdict == "dropped" & n_records > 0, "documented occurrence only", NA_character_))
}

# the cells the model calls present BEFORE the regional test: tests 1 and 3 ----
range_present_sql <- function(cell, thr) {
  glue::glue("
    SELECT c.taxon, c.cell_id, c.unit_id
    FROM {cell} c
    JOIN {thr}  t USING (taxon)
    WHERE c.val - COALESCE(c.spread, 0) >= t.thr
      AND NOT COALESCE(c.extrap, FALSE)")
}

#' SQL for the counts the regional test needs
#'
#' One row per taxon x region with the counts [region_effort_test()] takes. "Present" is the
#' model after tests 1 and 3 (value minus spread clears the threshold; not an extrapolation), so
#' the regional test judges the same cells the rule would otherwise keep. Counts are of distinct
#' `unit_id`: pass `unit_id = cell_id` to test at the cell, or a block id to test at a coarser
#' unit (records are clustered, and a block is a more honest trial than a 5 km cell).
#'
#' Tables (names are arguments; all keyed by `taxon`):
#'
#' | table | columns |
#' |---|---|
#' | `cell`   | `taxon, cell_id, unit_id, val, spread, extrap, is_ch` |
#' | `thr`    | `taxon, thr` |
#' | `region` | `region, cell_id` (not per taxon) |
#' | `effort` | `taxon, unit_id` -- units searched for this taxon (a record of its target group) |
#' | `occ`    | `taxon, unit_id` -- units holding a record of the taxon itself |
#'
#' A unit with a record of the species is searched by definition, whether or not `effort` lists it.
#'
#' @param cell,thr,region,effort,occ table (or view) names
#'
#' @return a SQL string: `taxon, region, n_cells, n_searched, n_occupied, n_records,
#'   n_searched_all, n_occupied_all`
#' @export
#' @concept range
range_region_sql <- function(
    cell = "og_cell", thr = "og_thr", region = "og_region", effort = "og_effort", occ = "og_occ") {
  glue::glue("
    WITH present AS ({range_present_sql(cell, thr)}),
    o AS (SELECT DISTINCT taxon, unit_id FROM {occ}),
    e AS (SELECT DISTINCT taxon, unit_id FROM {effort} UNION SELECT taxon, unit_id FROM o),
    unit AS (
      SELECT p.taxon, p.unit_id,
             (e.unit_id IS NOT NULL) AS searched,
             (o.unit_id IS NOT NULL) AS occupied
      FROM (SELECT DISTINCT taxon, unit_id FROM present) p
      LEFT JOIN e USING (taxon, unit_id)
      LEFT JOIN o USING (taxon, unit_id)),
    tot AS (
      SELECT taxon,
             count(*) FILTER (WHERE searched)              AS n_searched_all,
             count(*) FILTER (WHERE searched AND occupied) AS n_occupied_all
      FROM unit GROUP BY taxon),
    reg AS (
      SELECT p.taxon, r.region,
             count(DISTINCT p.cell_id)                                         AS n_cells,
             count(DISTINCT p.unit_id) FILTER (WHERE u.searched)               AS n_searched,
             count(DISTINCT p.unit_id) FILTER (WHERE u.searched AND u.occupied) AS n_occupied
      FROM present p
      JOIN {region} r USING (cell_id)
      JOIN unit     u USING (taxon, unit_id)
      GROUP BY p.taxon, r.region),
    rec AS (
      SELECT c.taxon, r.region, count(DISTINCT c.unit_id) AS n_records
      FROM {cell} c
      JOIN {region} r USING (cell_id)
      JOIN o USING (taxon, unit_id)
      GROUP BY c.taxon, r.region),
    base AS (
      SELECT DISTINCT c.taxon, r.region FROM {cell} c JOIN {region} r USING (cell_id))
    SELECT b.taxon, b.region,
           COALESCE(reg.n_cells, 0)        AS n_cells,
           COALESCE(reg.n_searched, 0)     AS n_searched,
           COALESCE(reg.n_occupied, 0)     AS n_occupied,
           COALESCE(rec.n_records, 0)      AS n_records,
           COALESCE(tot.n_searched_all, 0) AS n_searched_all,
           COALESCE(tot.n_occupied_all, 0) AS n_occupied_all
    FROM base b
    LEFT JOIN reg USING (taxon, region)
    LEFT JOIN rec USING (taxon, region)
    LEFT JOIN tot USING (taxon)
    ORDER BY b.taxon, b.region")
}

#' SQL for the range rule: which cells stay
#'
#' Applies the three tests to every cell of `cell` and says what happened to it. A cell is
#' `kept` when its value minus one bootstrap spread clears the taxon's threshold, it is not an
#' extrapolation, and none of its regions was dropped by the regional test. **Critical habitat is
#' never trimmed**: a critical-habitat cell that fails any test is `kept_ch`. Every other cell
#' carries the FIRST test it fails, in the order threshold, uncertainty, extrapolation, region:
#'
#' | `status` | meaning |
#' |---|---|
#' | `kept`              | passes all three tests |
#' | `kept_ch`           | fails a test, kept as critical habitat |
#' | `drop_no_threshold` | the taxon has no row in `thr` (never silently kept) |
#' | `drop_threshold`    | `val` is missing or below the threshold |
#' | `drop_uncertain`    | `val` clears the threshold, `val - spread` does not |
#' | `drop_extrapolated` | outside the environmental range the model was fitted on |
#' | `drop_region`       | in a region whose verdict is `'dropped'` |
#'
#' A missing `spread` counts as 0 and a missing `extrap` as `FALSE`: a layer the model run did
#' not produce cannot remove a cell. A cell in no region is judged by tests 1 and 3 alone.
#'
#' @param cell,thr,region table names, as in [range_region_sql()]
#' @param verdict table `taxon, region, verdict` ([region_effort_test()] of the counts)
#'
#' @return a SQL string: `taxon, cell_id, val, status, keep`, one row per row of `cell`
#' @export
#' @concept range
range_constrain_sql <- function(
    cell = "og_cell", thr = "og_thr", region = "og_region", verdict = "og_verdict") {
  glue::glue("
    WITH dropped AS (
      SELECT DISTINCT v.taxon, r.cell_id
      FROM {verdict} v
      JOIN {region}  r USING (region)
      WHERE v.verdict = 'dropped'),
    s AS (
      SELECT c.taxon, c.cell_id, c.val,
             COALESCE(c.is_ch, FALSE) AS is_ch,
             CASE
               WHEN t.thr IS NULL                         THEN 'drop_no_threshold'
               WHEN c.val IS NULL OR c.val < t.thr        THEN 'drop_threshold'
               WHEN c.val - COALESCE(c.spread, 0) < t.thr THEN 'drop_uncertain'
               WHEN COALESCE(c.extrap, FALSE)             THEN 'drop_extrapolated'
               WHEN d.cell_id IS NOT NULL                 THEN 'drop_region'
               ELSE 'kept'
             END AS test
      FROM {cell} c
      LEFT JOIN {thr}  t USING (taxon)
      LEFT JOIN dropped d USING (taxon, cell_id))
    SELECT taxon, cell_id, val,
           CASE WHEN test <> 'kept' AND is_ch THEN 'kept_ch' ELSE test END AS status,
           (test = 'kept' OR is_ch)                                        AS keep
    FROM s")
}

#' Convex hull of occurrence points, in the longitude frame that fits them
#'
#' The hull of a range that crosses the antimeridian must not wrap the long way round the globe.
#' The points are hulled in whichever longitude frame holds them in the narrower span
#' ([lon_span()]: -180..180, or 0..360 for a Pacific-centred range), and the hull is returned cut
#' at the antimeridian and shifted back, so it can be rasterized onto a -180..180 grid.
#'
#' The hull is planar in longitude / latitude, as OBIS's `convex_hull` mask is.
#'
#' @param lon,lat coordinates of the points (decimal degrees, -180..180)
#'
#' @return an `sfc` (EPSG:4326) of one POLYGON or MULTIPOLYGON; empty with fewer than 3 points
#' @importFrom sf st_sfc st_multipoint st_polygon st_convex_hull st_intersection st_union st_is_empty st_set_crs
#' @export
#' @concept range
range_hull <- function(lon, lat) {
  stopifnot(length(lon) == length(lat))
  ok  <- is.finite(lon) & is.finite(lat)
  lon <- lon[ok]; lat <- lat[ok]
  if (length(lon) < 3) return(st_sfc(st_polygon(), crs = 4326))
  span    <- lon_span(lon)
  wrapped <- span[2] > 180                    # the 0..360 frame is the narrower one
  x       <- if (wrapped) ifelse(lon < 0, lon + 360, lon) else lon
  # planar on purpose: no crs while hulling, so sf does not switch to the sphere
  hull <- st_convex_hull(st_sfc(st_multipoint(cbind(x, lat))))
  if (!wrapped) return(st_set_crs(hull, 4326))
  box  <- function(x0, x1) st_sfc(st_polygon(list(rbind(c(x0, -90), c(x1, -90), c(x1, 90), c(x0, 90), c(x0, -90)))))
  east <- st_intersection(hull, box(0, 180))             # stays where it is
  west <- st_intersection(hull, box(180, 360)) - c(360, 0) # back to -180..0
  parts <- c(east, west)
  st_set_crs(st_union(parts[!st_is_empty(parts)]), 4326)
}
