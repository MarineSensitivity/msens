# Taxon x zone exclusions -- the v7c rule that drops a taxon from an ecoregion where the
# observational record says it does not occur.
#
# v7c (title "v7.2") patches public v7 with two exclusions of one shape: a sea-turtle species is
# dropped from an ecoregion where it has never been recorded in any channel (sightings,
# strandings, bycatch, OBIS, GBIF, ...), and the North Atlantic right whale is dropped from the
# Gulf of America ecoregions. Both are rows of ONE evidence table (taxon x ecoregion), applied by
# ONE delete. The rule lives here, not in a notebook string, so `build_v7c.qmd` CALLS it and
# `test-exclude.R` ASSERTS it.

#' SQL assigning each cell to its single majority zone
#'
#' A cell on the boundary between two zones (1,125 of v7's 630,806 cells straddle two ecoregions)
#' belongs to the zone covering most of it, by `zone_cell.pct_covered`. A tie is broken
#' deterministically: the zone whose key (`zone.value`) sorts first wins, so the same inputs always
#' give the same assignment regardless of `zone_seq` order or scan order.
#'
#' Reads the v1-v7 scoring schema: `zone (zone_seq, tbl, fld, value, ...)` and
#' `zone_cell (zone_seq, cell_id, pct_covered)`; the zone key is `zone.value`. Only zones whose
#' `fld` equals `zone_fld` are considered. Should one key appear under several `zone_seq` (the same
#' field in two zone tables), its cover of a cell is collapsed by `max()` rather than summed, so a
#' duplicated vintage cannot double a zone's claim on a cell.
#'
#' A cell in no zone of `zone_fld` yields no row, so it can never be excluded by
#' [zone_exclude_sql()].
#'
#' @param zone_fld character; the `zone.fld` of the zone set, default `"ecoregion_key"`.
#' @param zone_cell character; name of the zone-cell relation, default `"zone_cell"`.
#' @param zone character; name of the zone relation, default `"zone"`.
#' @return SQL string selecting `(cell_id, zone_key)`, one row per cell, ordered by `cell_id`.
#' @examples
#' \dontrun{
#' con <- duckdb::dbConnect(duckdb::duckdb(), sdm_db_path("v7c"))
#' DBI::dbExecute(con, paste("CREATE TEMP TABLE cell_zone AS", cell_zone_major_sql()))
#' }
#' @concept merge
#' @importFrom glue glue
#' @export
cell_zone_major_sql <- function(
  zone_fld  = "ecoregion_key",
  zone_cell = "zone_cell",
  zone      = "zone") {

  stopifnot(
    is.character(zone_fld),  length(zone_fld)  == 1L, !is.na(zone_fld),  nzchar(zone_fld),
    is.character(zone_cell), length(zone_cell) == 1L, !is.na(zone_cell), nzchar(zone_cell),
    is.character(zone),      length(zone)      == 1L, !is.na(zone),      nzchar(zone))

  fld_sql <- .sql_str(zone_fld)

  # cover per (cell, zone key), then rank: highest cover first, the first key alphabetically on a tie
  glue::glue("
    WITH cover AS (
      SELECT zc.cell_id, z.value AS zone_key, max(zc.pct_covered) AS pct_covered
      FROM {zone_cell} zc
      JOIN {zone} z ON z.zone_seq = zc.zone_seq
      WHERE z.fld = {fld_sql}
      GROUP BY zc.cell_id, z.value
    ),
    ranked AS (
      SELECT cell_id, zone_key,
             row_number() OVER (
               PARTITION BY cell_id ORDER BY pct_covered DESC, zone_key ASC) AS rnk
      FROM cover
    )
    SELECT cell_id, zone_key
    FROM ranked
    WHERE rnk = 1
    ORDER BY cell_id",
    .trim = TRUE)
}

#' SQL selecting the model cells a taxon x zone exclusion removes
#'
#' v7c drops a taxon from a zone where the evidence says it does not occur, as rows of one
#' evidence table: sea turtles from the ecoregions where they have never been recorded in any
#' observational channel, and the North Atlantic right whale from the Gulf of America. A cell
#' straddling two zones goes with the zone covering most of it ([cell_zone_major_sql()]), and
#' critical habitat is never excluded: a cell the taxon holds under a designated critical habitat
#' (`ch_src`) survives inside an excluded zone.
#'
#' The result is the set of `(id_col, cell_id)` rows of `src` to DELETE, so the caller applies it as
#'
#' ```sql
#' DELETE FROM model_cell WHERE (mdl_seq, cell_id) IN (<zone_exclude_sql(...)>)
#' ```
#'
#' A model with no row in `exclusions` is never touched, and an exclusion for one model never
#' touches another model's rows in the same cell. The exclusions are inlined as a `VALUES` list,
#' so the SQL needs no temporary table for them.
#'
#' @param exclusions data frame with the id column named by `id_col` (`mdl_seq`, integer-like, or
#'   `mdl_key`, character) and `zone_key` (character, a `zone.value`); one row per excluded
#'   model x zone. Duplicate rows are collapsed.
#' @param src character; name of the model-cell relation holding `(id_col, cell_id, ...)`, default
#'   `"model_cell"`.
#' @param cell_zone character; name of the relation `(cell_id, zone_key)` giving each cell's
#'   majority zone, typically a temporary table built from [cell_zone_major_sql()].
#' @param ch_src character or `NULL`; name of a relation `(id_col, cell_id)` of critical-habitat
#'   cells per model. Rows listed there are never selected. `NULL` (default) = no guard.
#' @param id_col character; the model id column, `"mdl_seq"` (v1-v7, default) or `"mdl_key"` (v8+).
#' @return SQL string selecting distinct `(id_col, cell_id)`, ordered by `id_col, cell_id`.
#' @examples
#' zone_exclude_sql(
#'   data.frame(mdl_seq = c(54238L, 54252L), zone_key = c("EBS", "EGOA")),
#'   ch_src = "ch_cells")
#' @concept merge
#' @importFrom glue glue
#' @export
zone_exclude_sql <- function(
  exclusions,
  src       = "model_cell",
  cell_zone = "cell_zone",
  ch_src    = NULL,
  id_col    = "mdl_seq") {

  is_name <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  stopifnot(
    "`id_col` must be \"mdl_seq\" or \"mdl_key\"" =
      is_name(id_col) && id_col %in% c("mdl_seq", "mdl_key"),
    "`src` must be a non-empty string"           = is_name(src),
    "`cell_zone` must be a non-empty string"     = is_name(cell_zone),
    "`ch_src` must be NULL or a non-empty string" = is.null(ch_src) || is_name(ch_src),
    "`exclusions` must be a data frame"          = is.data.frame(exclusions),
    "`exclusions` must have at least one row"    = nrow(exclusions) > 0L)
  missing_cols <- setdiff(c(id_col, "zone_key"), names(exclusions))
  if (length(missing_cols) > 0L)
    stop(sprintf("`exclusions` lacks column(s): %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)

  ids  <- exclusions[[id_col]]
  keys <- exclusions[["zone_key"]]
  stopifnot(
    "`exclusions$zone_key` must be character without NA or empty values" =
      is.character(keys) && !anyNA(keys) && all(nzchar(keys)),
    "`exclusions` model ids must have no NA" = !anyNA(ids))
  if (id_col == "mdl_seq") {
    stopifnot(
      "`exclusions$mdl_seq` must be integer-like" =
        is.numeric(ids) && all(is.finite(ids)) && all(ids == round(ids)) &&
          all(abs(ids) <= .Machine$integer.max))
    id_lit <- format(as.integer(ids), scientific = FALSE, trim = TRUE)
  } else {
    stopifnot(
      "`exclusions$mdl_key` must be character without empty values" =
        is.character(ids) && all(nzchar(ids)))
    id_lit <- vapply(ids, .sql_str, "", USE.NAMES = FALSE)
  }

  rows       <- unique(paste0("(", id_lit, ", ", vapply(keys, .sql_str, "", USE.NAMES = FALSE), ")"))
  values_sql <- paste(rows, collapse = ",\n  ")

  # critical habitat is never excluded: a (model, cell) carried by ch_src is kept whatever its zone
  ch_sql <- if (is.null(ch_src)) "" else paste0(
    "\nWHERE NOT EXISTS (\n",
    "  SELECT 1 FROM ", ch_src, " ch\n",
    "  WHERE ch.", id_col, " = s.", id_col, " AND ch.cell_id = s.cell_id)")

  glue::glue("
    WITH ex({id_col}, zone_key) AS (
      VALUES
      {values_sql}
    )
    SELECT DISTINCT s.{id_col}, s.cell_id
    FROM {src} s
    JOIN {cell_zone} cz ON cz.cell_id = s.cell_id
    JOIN ex ON ex.{id_col} = s.{id_col} AND ex.zone_key = cz.zone_key{ch_sql}
    ORDER BY s.{id_col}, s.cell_id",
    .trim = TRUE)
}
