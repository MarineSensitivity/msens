# One occurrence (vagrant) rule for every species (v11 Q2; plan 2026-10-29 §4.3, decisions D10/D11).
#
# v7c removed two things by hand-evidenced rows: the sea turtles from the East Bering Sea (zero
# records in nine channels) and the North Atlantic right whale from the Gulf of America (an
# "outside range" judgment). Tim White (2026-10-21): one written rule that treats both the same way,
# with tiers -- protected places override, "unsampled is unknown, not absent", wide-ranging taxa.
# The general candidate is rule E of `obis_range_constraint`, [region_effort_test()]: per region,
# the species' modelled units that were SEARCHED (a record of its effort group exists) against
# those that hold a record of the species, judged against the species' record rate everywhere
# else; a region is dropped only when records are significantly AND materially fewer, kept when
# nobody looked. This file turns that test into the rule the merge consumes: counts from the
# relations the pipeline has, the overrides as data, verdicts per species x zone, and the rows
# [zone_exclude_sql()] deletes. `explore_occurrence_rule.qmd` / `merge_models.qmd` CALL it under
# OCC_RULE=1; `test-occurrence.R` ASSERTS it.

#' SQL for the counts the occurrence rule needs, per species x zone
#'
#' One row per (model, zone) with the counts [region_effort_test()] takes, on `block`-wide units
#' (default 10 cells = 0.5 degrees on `global05`: records cluster, so a block is a more honest
#' trial than a 5 km cell). Grid-agnostic through `nc` (the grid's column count) and `block`.
#'
#' Relations (names are arguments; `mdl_key` keys the species):
#'
#' | relation | columns | meaning |
#' |---|---|---|
#' | `present`    | `mdl_key, cell_id`          | the cells the model calls present, over the WHOLE modelled range (the merged global surface), so a zone is judged against the species' stronghold rate elsewhere |
#' | `occ`        | `mdl_key, cell_id, records` | records of the species per cell (OBIS `obis_cell`, through the crosswalk) |
#' | `effort`     | `mdl_key, cell_id`          | cells searched for the species' effort group (a record of its order, or of any air-breathing megafauna) |
#' | `region`     | `zone_key, cell_id`         | the zone each cell belongs to (majority zone, [cell_zone_major_sql()]); PRESENCE is counted here |
#' | `region_rec` | `zone_key, cell_id`         | the zone dilated by the buffer `d` (a record just outside the zone counts); RECORDS and EFFORT are counted here; default = `region` |
#'
#' A unit with a record of the species is searched by definition, whether or not `effort` lists it.
#'
#' @param present,occ,effort,region,region_rec relation names.
#' @param nc integer; the grid's number of columns (7200 for `global05`).
#' @param block integer; cells per block side (10 = 0.5 degrees).
#' @return SQL selecting `mdl_key, zone_key, n_cells, n_searched, n_occupied, n_records,
#'   n_searched_all, n_occupied_all`, one row per (model, zone) where the model is present.
#' @concept range
#' @importFrom glue glue
#' @export
occurrence_counts_sql <- function(
  present = "occ_present", occ = "occ_records", effort = "occ_effort",
  region = "occ_region", region_rec = region, nc = 7200L, block = 10L) {

  stopifnot(
    is.numeric(nc), length(nc) == 1L, nc >= 1, is.numeric(block), length(block) == 1L, block >= 1,
    vapply(list(present, occ, effort, region, region_rec), function(x) is.character(x) && length(x) == 1L && nzchar(x), TRUE))
  nc <- as.integer(nc); block <- as.integer(block)
  # the cell id is cast to BIGINT first: DuckDB's `//` on a DOUBLE column (a data frame's numeric
  # cell_id) is not the integer floor division the block arithmetic needs
  unit <- function(id) glue::glue(
    "CAST((((CAST({id} AS BIGINT) - 1) // {nc}) // {block}) * {nc %/% block} + ((CAST({id} AS BIGINT) - 1) % {nc}) // {block} AS INTEGER)")

  glue::glue("
    WITH p AS (
      SELECT DISTINCT mdl_key, cell_id, {unit('cell_id')} AS unit_id FROM {present}),
    o AS (
      SELECT DISTINCT mdl_key, {unit('cell_id')} AS unit_id FROM {occ} WHERE records > 0),
    e AS (
      SELECT DISTINCT mdl_key, {unit('cell_id')} AS unit_id FROM {effort}
      UNION SELECT mdl_key, unit_id FROM o),
    unit AS (
      SELECT pu.mdl_key, pu.unit_id,
             (e.unit_id IS NOT NULL) AS searched,
             (o.unit_id IS NOT NULL) AS occupied
      FROM (SELECT DISTINCT mdl_key, unit_id FROM p) pu
      LEFT JOIN e USING (mdl_key, unit_id)
      LEFT JOIN o USING (mdl_key, unit_id)),
    tot AS (
      SELECT mdl_key,
             count(*) FILTER (WHERE searched)              AS n_searched_all,
             count(*) FILTER (WHERE searched AND occupied) AS n_occupied_all
      FROM unit GROUP BY mdl_key),
    -- presence in the zone proper; effort and records over the buffered zone
    zr AS (SELECT DISTINCT zone_key, {unit('cell_id')} AS unit_id FROM {region_rec}),
    reg AS (
      SELECT p.mdl_key, r.zone_key,
             count(DISTINCT p.cell_id) AS n_cells,
             count(DISTINCT p.unit_id) FILTER (WHERE u.searched)              AS n_searched,
             count(DISTINCT p.unit_id) FILTER (WHERE u.searched AND u.occupied) AS n_occupied
      FROM p
      JOIN {region} r USING (cell_id)
      JOIN unit u USING (mdl_key, unit_id)
      GROUP BY p.mdl_key, r.zone_key),
    rec AS (
      SELECT o.mdl_key, zr.zone_key, count(DISTINCT o.unit_id) AS n_records
      FROM o JOIN zr USING (unit_id)
      GROUP BY o.mdl_key, zr.zone_key)
    SELECT reg.mdl_key, reg.zone_key, reg.n_cells, reg.n_searched, reg.n_occupied,
           COALESCE(rec.n_records, 0)      AS n_records,
           COALESCE(tot.n_searched_all, 0) AS n_searched_all,
           COALESCE(tot.n_occupied_all, 0) AS n_occupied_all
    FROM reg
    LEFT JOIN rec USING (mdl_key, zone_key)
    LEFT JOIN tot USING (mdl_key)
    ORDER BY reg.mdl_key, reg.zone_key", .trim = TRUE)
}

#' The occurrence rule: per species x zone, kept / dropped / unsearched, with the overrides
#'
#' Runs [occurrence_counts_sql()] on `con`, judges every (species, zone) with
#' [region_effort_test()] (`alpha`, `ratio`), then applies the overrides Tim White asked for
#' (2026-10-21), each recorded in `override` so the report can say why a zone was kept:
#' \itemize{
#'   \item `"protected"` -- the species holds designated cells (critical habitat, core areas: the
#'     `ch` relation) inside the zone: never dropped;
#'   \item `"narrow range"` -- the species' modelled range has fewer than `min_cells` cells: a
#'     narrow-range species is never dropped by a record test (the test needs a stronghold to
#'     compare with);
#'   \item `"unsearched"` -- nobody looked (the test's own verdict): kept and flagged, since empty
#'     of observers is not empty of the species.
#' }
#' A zone is `"dropped"` only when it was searched, holds significantly and materially fewer records
#' than the species' rate elsewhere, and no override applies; `label` says `"documented occurrence
#' only"` where a dropped zone still has a record (the vagrant is on the record, the zone is not
#' range). Zones with no present cell of the species get no row.
#'
#' @inheritParams occurrence_counts_sql
#' @param con a DBI connection holding the relations.
#' @param ch character or `NULL`; a relation `(mdl_key, cell_id)` of protected cells per model.
#' @param alpha,ratio passed to [region_effort_test()].
#' @param min_cells integer; species with fewer present cells (worldwide) are never dropped.
#' @return a data frame, one row per (model, zone): the counts, `p_core, rate, n_min, p_value`,
#'   `verdict` (`kept | dropped | unsearched`), `label`, `override` (`NA | protected | narrow range`),
#'   and `verdict_raw` (the test's verdict before overrides).
#' @concept range
#' @importFrom DBI dbGetQuery
#' @export
occurrence_rule <- function(
  con,
  present = "occ_present", occ = "occ_records", effort = "occ_effort",
  region = "occ_region", region_rec = region, ch = NULL,
  nc = 7200L, block = 10L, alpha = 0.05, ratio = 0.25, min_cells = 20L) {

  stopifnot(
    is.null(ch) || (is.character(ch) && length(ch) == 1L && nzchar(ch)),
    is.numeric(min_cells), length(min_cells) == 1L, min_cells >= 0)
  d <- DBI::dbGetQuery(con, occurrence_counts_sql(present, occ, effort, region, region_rec, nc, block))
  if (nrow(d) == 0L) {
    out <- cbind(d, data.frame(p_core = numeric(), rate = numeric(), n_min = numeric(), p_value = numeric(),
                               verdict_raw = character(), verdict = character(), label = character(),
                               override = character(), stringsAsFactors = FALSE))
    return(out)
  }
  t <- region_effort_test(d$n_searched, d$n_occupied, d$n_searched_all, d$n_occupied_all,
                          n_records = d$n_records, alpha = alpha, ratio = ratio)
  d$p_core <- t$p_core; d$rate <- t$rate; d$n_min <- t$n_min; d$p_value <- t$p_value
  d$verdict_raw <- t$verdict; d$label <- t$label
  d$override <- NA_character_

  # narrow range: the species' whole modelled range, in cells
  n_all <- DBI::dbGetQuery(con, glue::glue("SELECT mdl_key, count(DISTINCT cell_id) AS n FROM {present} GROUP BY mdl_key"))
  narrow <- n_all$mdl_key[n_all$n < min_cells]
  d$override[d$mdl_key %in% narrow & d$verdict_raw == "dropped"] <- "narrow range"
  # protected places: a zone holding designated cells of the species is never dropped
  if (!is.null(ch)) {
    prot <- DBI::dbGetQuery(con, glue::glue(
      "SELECT DISTINCT c.mdl_key, r.zone_key FROM {ch} c JOIN {region} r USING (cell_id)"))
    is_prot <- paste(d$mdl_key, d$zone_key) %in% paste(prot$mdl_key, prot$zone_key)
    d$override[is_prot & d$verdict_raw == "dropped"] <- "protected"
  }
  d$verdict <- ifelse(!is.na(d$override), "kept", d$verdict_raw)
  d$label[d$verdict != "dropped"] <- NA_character_
  d
}

#' The exclusion rows the occurrence rule hands to the merge
#'
#' @param d the result of [occurrence_rule()].
#' @return a data frame `(mdl_key, zone_key)` of the dropped (model, zone) pairs, the shape
#'   [zone_exclude_sql()] takes (`id_col = "mdl_key"`); zero rows when nothing is dropped.
#' @concept range
#' @export
occurrence_exclusions <- function(d) {
  stopifnot(is.data.frame(d), all(c("mdl_key", "zone_key", "verdict") %in% names(d)))
  x <- d[d$verdict == "dropped", c("mdl_key", "zone_key"), drop = FALSE]
  rownames(x) <- NULL
  x
}
