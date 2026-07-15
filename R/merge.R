#' v8 per-taxon merge rules (single source of truth)
#'
#' The SQL that defines how per-dataset model cells are merged into one surface per taxon.
#' `merge_models.qmd` executes these strings and `tests/testthat/test-merge.R` asserts them
#' against synthetic fixtures, so the notebook and the tests can never drift. Changing a rule
#' here that breaks a documented case fails the unit tests — the guard against the merge logic
#' being silently lost by a wrong sequence or minor tweak.
#'
#' The batch caller must first materialize three input relations in the connection:
#' \describe{
#'   \item{`b`}{`(ms_merge_key, ds_key, cell_id, val)` — the raw model cells for a batch of taxa
#'     (all datasets; `ds_key = 'am'` is AquaMaps, anything else is a range/expert dataset).}
#'   \item{`taxon`}{`(ms_merge_key, er_score, ...)` — governing extinction-risk score per taxon.}
#'   \item{`taxon_flags`}{`(ms_merge_key, has_am, has_range)` — GLOBAL presence flags from the
#'     crosswalk `taxon_model` (has_range = the taxon has ANY non-am dataset anywhere, NOT just in
#'     the US). This global scope is what enforces the IUCN-range constraint — see below.}
#'   \item{`us_cells`}{`(cell_id)` — the in-USA cell ids (scoring extent).}
#' }
#'
#' `merge_sql()$b_range`, `$b_am_all`, `$b_am_rng` create the intermediates; then `$global` and
#' `$us` are the two output surfaces:
#'
#' \strong{GLOBAL viz surface} (`$global`) = am ∪ range (FULL OUTER of the range footprint valued by
#' governing er and the taxon's WHOLE am footprint). am-only taxa are omitted (they reuse am COGs).
#' This is the honest whole-range merged model painted to COGs.
#'
#' \strong{US scoring surface} (`$us`) = v7-faithful, US-boundary-aware, IUCN-CONSTRAINED:
#' \enumerate{
#'   \item range footprint ∩ US valued `max(er, am-at-range)` — am BEYOND the range is MASKED (the
#'     expert range constrains AquaMaps over-prediction). Covers range-only + "both" taxa.
#'   \item TRUE am-only taxa (GLOBAL `has_range = FALSE`, i.e. no range dataset anywhere) keep their
#'     RAW am ∩ US (no dedup — a taxon with >1 AquaMaps model keeps duplicate cells).
#' }
#' A species that HAS an expert range whose polygons lie ENTIRELY outside the US therefore gets NO
#' US presence: its range ∩ US is empty (1) and it is excluded from (2) because global has_range is
#' TRUE. This is the `iucn_range_outside_us_eez` exclusion (e.g. \emph{Sotalia guianensis}, a river
#' dolphin AquaMaps over-predicts into US waters). Keying (2) on GLOBAL has_range — not range-in-US —
#' is the crux; keying it on range-in-US silently re-introduces ~750 such species.
#'
#' @return named list of SQL strings: `b_range`, `b_am_all`, `b_am_rng` (CREATE OR REPLACE TABLE),
#'   `global` and `us` (SELECT).
#' @concept merge
#' @export
#' @examples
#' \dontrun{
#'   msq <- merge_sql()
#'   DBI::dbExecute(con, msq$b_range); DBI::dbExecute(con, msq$b_am_all)
#'   DBI::dbExecute(con, msq$b_am_rng)
#'   us <- DBI::dbGetQuery(con, msq$us)      # US scoring surface for the batch
#'   gl <- DBI::dbGetQuery(con, msq$global)  # global whole-range surface for the batch
#' }
merge_sql <- function() {
  list(
    # range footprint (non-am) valued by the taxon's governing er_score
    b_range = paste(
      "CREATE OR REPLACE TABLE b_range AS",
      "SELECT DISTINCT b.ms_merge_key, b.cell_id, t.er_score::DOUBLE AS er",
      "FROM b JOIN taxon t ON b.ms_merge_key = t.ms_merge_key WHERE b.ds_key <> 'am'"),
    # (global) am over the WHOLE am footprint of has_range taxa -> am∪range viz surface
    b_am_all = paste(
      "CREATE OR REPLACE TABLE b_am_all AS",
      "SELECT b.ms_merge_key, b.cell_id, max(b.val) am_val",
      "FROM b JOIN taxon_flags tf ON b.ms_merge_key = tf.ms_merge_key",
      "WHERE b.ds_key = 'am' AND tf.has_range GROUP BY 1, 2"),
    # (US) am AT range cells only -> range-footprint max(er, am-at-range)
    b_am_rng = paste(
      "CREATE OR REPLACE TABLE b_am_rng AS",
      "SELECT b.ms_merge_key, b.cell_id, max(b.val) am_val",
      "FROM b JOIN b_range USING (ms_merge_key, cell_id) WHERE b.ds_key = 'am' GROUP BY 1, 2"),
    # GLOBAL viz surface = am ∪ range (FULL OUTER)
    global = paste(
      "SELECT ms_merge_key AS mdl_key, cell_id,",
      "greatest(coalesce(br.er, 0), coalesce(ba.am_val, 0))::DOUBLE AS val",
      "FROM b_range br FULL OUTER JOIN b_am_all ba USING (ms_merge_key, cell_id)"),
    # US scoring surface = (A) range∩US max(er, am-at-range) UNION (B) raw am∩US for TRUE am-only taxa
    us = paste(
      "SELECT br.ms_merge_key AS mdl_key, br.cell_id,",
      "       greatest(br.er, coalesce(ba.am_val, 0))::DOUBLE AS val",
      "FROM b_range br JOIN us_cells u ON br.cell_id = u.cell_id",
      "LEFT JOIN b_am_rng ba ON ba.ms_merge_key = br.ms_merge_key AND ba.cell_id = br.cell_id",
      "UNION ALL",
      "SELECT b.ms_merge_key AS mdl_key, b.cell_id, b.val::DOUBLE AS val",
      "FROM b JOIN us_cells u ON b.cell_id = u.cell_id",
      "WHERE b.ds_key = 'am' AND b.ms_merge_key IN (SELECT ms_merge_key FROM taxon_flags WHERE NOT has_range)")
  )
}

#' Turtle multiplicative merge rule
#'
#' Sea turtles merge differently: the DPS extinction-risk surface (`turtle_ds`) is multiplied by the
#' AquaMaps suitability (`suit_ds`), floored at 1 over the ER footprint, then critical-habitat
#' datasets (`ch_keys`) override with a max. `val = greatest(1, round(er * suit / 100))` then
#' `greatest(that, ch)`. Reads a source relation `src` with `(ms_merge_key, ds_key, cell_id, val)`.
#'
#' @param turtle_ds character; ds_key of the turtle DPS extinction-risk dataset.
#' @param suit_ds character; ds_key of the AquaMaps suitability dataset (usually `"am"`).
#' @param ch_keys character vector of critical-habitat ds_keys that override with a max (may be empty).
#' @param src character; name of the source relation to read (default `"turtle_src"`).
#' @return SQL string selecting `(mdl_key, cell_id, val)` — the whole-range turtle surface.
#' @concept merge
#' @importFrom glue glue
#' @export
turtle_sql <- function(turtle_ds, suit_ds, ch_keys, src = "turtle_src") {
  ch_sql <- if (length(ch_keys)) paste(sprintf("'%s'", ch_keys), collapse = ", ") else "''"
  glue::glue(
    "WITH er   AS (SELECT ms_merge_key, cell_id, max(val) er_value   FROM {src} WHERE ds_key = '{turtle_ds}' GROUP BY 1, 2),\n",
    "     suit AS (SELECT ms_merge_key, cell_id, max(val) suit_value FROM {src} WHERE ds_key = '{suit_ds}'   GROUP BY 1, 2),\n",
    "     ch   AS (SELECT ms_merge_key, cell_id, max(val) ch_value   FROM {src} WHERE ds_key IN ({ch_sql})   GROUP BY 1, 2),\n",
    "     mult AS (SELECT er.ms_merge_key, er.cell_id,\n",
    "                greatest(1, CAST(round(er.er_value * coalesce(suit.suit_value, 1) / 100.0) AS INTEGER)) AS val\n",
    "              FROM er LEFT JOIN suit USING (ms_merge_key, cell_id))\n",
    "SELECT m.ms_merge_key AS mdl_key, m.cell_id,\n",
    "       CAST(greatest(m.val, coalesce(ch.ch_value, 0)) AS DOUBLE) AS val\n",
    "FROM mult m LEFT JOIN ch USING (ms_merge_key, cell_id)",
    .trim = FALSE)
}
