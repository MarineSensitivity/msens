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
#'   \item{`taxon_flags`}{`(ms_merge_key, has_suit, has_range)` — GLOBAL presence flags from the
#'     crosswalk `taxon_model` (has_range = the taxon has ANY non-suitability dataset anywhere, NOT
#'     just in the US). This global scope is what enforces the IUCN-range constraint — see below.}
#'   \item{`us_cells`}{`(cell_id)` — the in-USA cell ids (scoring extent).}
#' }
#'
#' `merge_sql()$b_range` and `$b_am_rng` create the intermediates; then `$global` and `$us` are the
#' two output surfaces. Both apply the SAME range mask — they differ only in extent (global vs
#' US) and in the am-only branch that only the US surface needs:
#'
#' \strong{GLOBAL viz surface} (`$global`) = the taxon's WHOLE range footprint valued
#' `max(er, am-at-range)` — am BEYOND the range is MASKED, exactly as in `$us` but without the US
#' trim. am-only taxa are omitted (they reuse am COGs). This is the whole-range merged model
#' painted to COGs, and it is what the species app draws.
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
#' The mask must hold on BOTH surfaces. `$global` was briefly a FULL OUTER union of the range with
#' the whole am footprint, which painted raw AquaMaps over-prediction into every merged COG — half
#' the walrus surface, reaching 9.75 degrees N — while `$us` stayed correct and the manifest hash,
#' which fingerprints only `$us`, could not see it (MarineSensitivity/apps#8). `test-merge.R` now
#' asserts `$global` has no cell outside the range footprint.
#'
#' \strong{Which dataset is "suitability"} is the `suit_ds` argument: `"am"` alone through v8;
#' `c("am", "ax")` from v9, when AquaX joins AquaMaps as a second suitability source. Every other
#' `ds_key` is a range/expert dataset. Two suitability datasets for one taxon at one cell would
#' simply `max()` here — which is why supersession (AquaX replacing AquaMaps inside the AquaX
#' mask) is applied \emph{before} this, on the merge input, by [supersede_sql()]: this function
#' never sees an `am` cell it should ignore.
#'
#' @param suit_ds character vector of suitability dataset keys (default `"am"`)
#' @return named list of SQL strings: `b_range`, `b_am_rng` (CREATE OR REPLACE TABLE),
#'   `global` and `us` (SELECT).
#' @concept merge
#' @export
#' @examples
#' \dontrun{
#'   msq <- merge_sql(c("am", "ax"))
#'   DBI::dbExecute(con, msq$b_range); DBI::dbExecute(con, msq$b_am_rng)
#'   us <- DBI::dbGetQuery(con, msq$us)      # US scoring surface for the batch
#'   gl <- DBI::dbGetQuery(con, msq$global)  # global whole-range surface for the batch
#' }
merge_sql <- function(suit_ds = "am") {
  stopifnot(is.character(suit_ds), length(suit_ds) >= 1, all(nzchar(suit_ds)))
  suit <- paste(sprintf("'%s'", suit_ds), collapse = ", ")
  list(
    # range footprint (non-suitability datasets) valued by the taxon's governing er_score
    b_range = paste(
      "CREATE OR REPLACE TABLE b_range AS",
      "SELECT DISTINCT b.ms_merge_key, b.cell_id, t.er_score::DOUBLE AS er",
      "FROM b JOIN taxon t ON b.ms_merge_key = t.ms_merge_key",
      sprintf("WHERE b.ds_key NOT IN (%s)", suit)),
    # suitability AT range cells only -> range-footprint max(er, suit-at-range). The JOIN to
    # b_range IS the mask: suitability beyond the expert range never enters either output surface,
    # and suit-only taxa (no b_range rows) drop out of the global surface entirely. (The table keeps
    # its historical name b_am_rng; from v9 it holds whichever suitability dataset survives
    # supersession at that cell.)
    b_am_rng = paste(
      "CREATE OR REPLACE TABLE b_am_rng AS",
      "SELECT b.ms_merge_key, b.cell_id, max(b.val) am_val",
      "FROM b JOIN b_range USING (ms_merge_key, cell_id)",
      sprintf("WHERE b.ds_key IN (%s) GROUP BY 1, 2", suit)),
    # GLOBAL viz surface = whole range footprint, masked: max(er, am-at-range)
    global = paste(
      "SELECT br.ms_merge_key AS mdl_key, br.cell_id,",
      "       greatest(br.er, coalesce(ba.am_val, 0))::DOUBLE AS val",
      "FROM b_range br",
      "LEFT JOIN b_am_rng ba ON ba.ms_merge_key = br.ms_merge_key AND ba.cell_id = br.cell_id"),
    # US scoring surface = (A) range∩US max(er, am-at-range) UNION (B) raw am∩US for TRUE am-only taxa
    us = paste(
      "SELECT br.ms_merge_key AS mdl_key, br.cell_id,",
      "       greatest(br.er, coalesce(ba.am_val, 0))::DOUBLE AS val",
      "FROM b_range br JOIN us_cells u ON br.cell_id = u.cell_id",
      "LEFT JOIN b_am_rng ba ON ba.ms_merge_key = br.ms_merge_key AND ba.cell_id = br.cell_id",
      "UNION ALL",
      "SELECT b.ms_merge_key AS mdl_key, b.cell_id, b.val::DOUBLE AS val",
      "FROM b JOIN us_cells u ON b.cell_id = u.cell_id",
      sprintf("WHERE b.ds_key IN (%s)", suit),
      "AND b.ms_merge_key IN (SELECT ms_merge_key FROM taxon_flags WHERE NOT has_range)")
  )
}

#' Supersession predicate: one suitability dataset replaces another inside a mask
#'
#' From v9, AquaX (`ax`) is a newer model of the same thing as AquaMaps (`am`) — but it was
#' delivered only over the US study area (its own mask: the union over models of their modeled
#' pixels — one model's NA area is its range crop, i.e. the model saying absent — so the mask is
#' the union, measured against `cell.in_usa` by the ingest). The rule is therefore \strong{per
#' taxon, inside the mask}: for a taxon that has a superseding model, the superseded dataset's
#' cells that fall inside the mask are dropped from the merge input; outside the mask (the rest
#' of the world, and any US cell no model reaches) the old dataset carries on. Applied to BOTH output surfaces, since it filters the input they share.
#'
#' Why a filter on the input rather than a `coalesce()` inside [merge_sql()]: a per-cell coalesce
#' would keep an AquaMaps cell wherever AquaX has \emph{no} value — i.e. exactly where the newer
#' model says the species is absent — which defeats the point of superseding it. Dropping the old
#' cells over the whole mask lets AquaX's absences be absences.
#'
#' Which taxa are superseded is a TABLE the caller materializes (`taxa`, one `ms_merge_key` per
#' row), not derived here from "has a superseding model", because the policy question — does an
#' AquaX run that predicted \emph{no} US presence also supersede? (v9: no, `AX_ABSENT_SUPERSEDES`)
#' — belongs in the notebook, where it is a flag and a table a reviewer can read.
#'
#' @param superseded ds_key whose cells are dropped inside the mask (default `"am"`)
#' @param taxa relation holding `ms_merge_key` of the taxa to supersede (default `"supersede"`)
#' @param mask relation holding the `cell_id`s of the superseding dataset's extent (default `"ax_mask"`)
#' @param src alias of the relation being filtered, with `ds_key`, `ms_merge_key`, `cell_id`
#' @return a SQL predicate (no leading `WHERE`) that is TRUE for rows to KEEP
#' @concept merge
#' @export
#' @examples
#' supersede_sql()
#' \dontrun{ DBI::dbGetQuery(con, paste("SELECT * FROM mc WHERE", supersede_sql(src = "mc"))) }
supersede_sql <- function(superseded = "am", taxa = "supersede", mask = "ax_mask", src = "mc") {
  stopifnot(is.character(superseded), length(superseded) == 1, nzchar(superseded))
  sprintf(paste(
    "NOT (%s.ds_key = '%s'",
    "AND %s.ms_merge_key IN (SELECT ms_merge_key FROM %s)",
    "AND %s.cell_id IN (SELECT cell_id FROM %s))"),
    src, superseded, src, taxa, src, mask)
}

#' Spatial-ER (turtle) multiplicative merge rule
#'
#' Taxa whose extinction risk is SPATIAL — a per-cell ER surface built from Distinct Population
#' Segment polygons (sea turtles from SWOT + NMFS DPS since v4b; from v9.1 any species with NMFS
#' DPS boundaries, e.g. the humpback whale, via `dps_nmfs`) — merge differently: the per-cell ER
#' surface (`turtle_ds`, one or more datasets) is multiplied by the suitability (`suit_ds`),
#' floored at 1 over the ER footprint, then critical-habitat datasets (`ch_keys`) override with a
#' max. `val = greatest(1, round(er * suit / 100))` then `greatest(that, ch)`. Reads a source
#' relation `src` with `(ms_merge_key, ds_key, cell_id, val)`.
#'
#' This is what keeps an Endangered species from painting flat: the plain rule values every
#' range cell at the taxon's governing ER (`max(er, suit)` = 100 everywhere for `NMFS:EN`), while
#' here the ER varies by DPS and the suitability still shows through.
#'
#' @param turtle_ds character vector; ds_key(s) of the per-cell extinction-risk dataset(s).
#' @param suit_ds character vector; ds_key(s) of the suitability dataset(s) (`"am"`, or
#'   `c("am", "ax")` from v9 — after [supersede_sql()] at most one survives per cell, and `max()`
#'   over the survivors is the value).
#' @param ch_keys character vector of critical-habitat ds_keys that override with a max (may be empty).
#' @param src character; name of the source relation to read (default `"turtle_src"`).
#' @return SQL string selecting `(mdl_key, cell_id, val)` — the whole-range turtle surface.
#' @concept merge
#' @importFrom glue glue
#' @export
turtle_sql <- function(turtle_ds, suit_ds, ch_keys, src = "turtle_src") {
  ch_sql <- if (length(ch_keys)) paste(sprintf("'%s'", ch_keys), collapse = ", ") else "''"
  suit_sql <- paste(sprintf("'%s'", suit_ds), collapse = ", ")
  er_sql   <- paste(sprintf("'%s'", turtle_ds), collapse = ", ")
  glue::glue(
    "WITH er   AS (SELECT ms_merge_key, cell_id, max(val) er_value   FROM {src} WHERE ds_key IN ({er_sql}) GROUP BY 1, 2),\n",
    "     suit AS (SELECT ms_merge_key, cell_id, max(val) suit_value FROM {src} WHERE ds_key IN ({suit_sql}) GROUP BY 1, 2),\n",
    "     ch   AS (SELECT ms_merge_key, cell_id, max(val) ch_value   FROM {src} WHERE ds_key IN ({ch_sql})   GROUP BY 1, 2),\n",
    "     mult AS (SELECT er.ms_merge_key, er.cell_id,\n",
    "                greatest(1, CAST(round(er.er_value * coalesce(suit.suit_value, 1) / 100.0) AS INTEGER)) AS val\n",
    "              FROM er LEFT JOIN suit USING (ms_merge_key, cell_id))\n",
    "SELECT m.ms_merge_key AS mdl_key, m.cell_id,\n",
    "       CAST(greatest(m.val, coalesce(ch.ch_value, 0)) AS DOUBLE) AS val\n",
    "FROM mult m LEFT JOIN ch USING (ms_merge_key, cell_id)",
    .trim = FALSE)
}
