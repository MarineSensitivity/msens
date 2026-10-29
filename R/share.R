# The v10 distribution-share score (single source of truth).
#
# Each scored species' value in a cell is divided by that species' total over its WHOLE modelled
# range, risk-weighted, summed per component and zone, and expressed per km2 against the national
# average (100 = the national average concentration), with the percent of the world's importance
# beside it, equal component weights, alpha = 0 and the kappa = 5 % coverage floor -- the method of
# the review page "Sensitivity Across Regions" (artifact 773b9b71, rev 14, 2026-09-11), formulas
# v10.1-v10.4, companion and composite, plus Tim White's 2026-09-11 brief (the US-EEZ denominator as
# a species weight, section 13a). The rule lives here, not in a notebook string, so
# `score_share_metrics.qmd` CALLS it and `test-share.R` ASSERTS it.
#
#   share_s,c   = v_s,c * A_c / D_s                     D_s = sum over range(s) of v * A  (v10.1)
#   share_c,g   = sum_{s in g} w_s,c * share_s,c / 100   w = er_score | 100 (premultiplied) | er_s,c (cell)  (v10.2)
#   share_z,g   = sum_c share_c,g * p_zc                 p_zc = zone_cell.pct_covered / 100               (v10.3)
#   v10_z,g     = 100 * (share_z,g / share_USA,g) / (A_z / A_USA)                                       (v10.4)
#   pct_world   = 100 * share_z,g / (sum_{s in g} w_s / 100)        (global denominator only)
#   pct_nation  = 100 * share_z,g / share_USA,g                     (every denominator)
#   coverage    = area of the zone's cells carrying any species of g / A_z; reportable iff >= kappa
#   composite_z = mean over reportable components of v10_z,g (+ primprod_v10 = 100 * mean_z / mean_USA)
#
# Three denominators, one flag (plan 2026-10-29 D2): `global` (D_s over the species' whole modelled
# range: the headline), `usa` (D_s over its US cells: every species sums to 1 inside the country --
# the brief's choice, = the global score with species weight w instead of w * f_us), `pooled` (no
# per-species normalization: RESA-style value * area summed, the national reference divides it out).
#
# The risk weight by `taxon.er_mode` (what score_cell_metrics.qmd does, and the page's Symbols):
#   taxon         w_s,c = er_score, the governing scalar; the species' total weight is er_score / 100
#   premultiplied the merged value is already ER x suitability (sea turtles, msens::turtle_sql); w = 100
#                 passes it through. Because the share normalizes v by sum(v), the ER LEVEL cancels and
#                 the species' total weight is 1 whatever its status -- only its spatial pattern survives.
#                 Every v8 turtle has er_score 100, so v10 is unaffected; v11 moves the turtles to the
#                 DPS path, which dissolves the case.
#   cell          the merged value is the suitability alone and the per-cell ER rides beside it
#                 (NMFS DPS species, msens::dps_sql, v9+): w_s,c = er_s,c on the share of the PLAIN
#                 suitability total, so the species' total weight is its suitability-weighted mean ER
#                 (= er_score / 100 when the ER is uniform, consistent with the taxon mode). The v10-1
#                 subplan had proposed an ER-carrying denominator instead, which would give every DPS
#                 species total weight 1 like a turtle; the page's Symbols were followed (decision
#                 flagged for Ben 2026-10-29). No v10 taxon is in this mode.

#' SQL for the v10 distribution-share score family
#'
#' Returns the SQL strings that compute the share score from the scoring schema. The caller (normally
#' [share_run()]) materializes these relations in the connection first:
#' \describe{
#'   \item{`model_cell`}{`(mdl_key, cell_id, <val_col>)` -- the US scoring surface, one row per (taxon,
#'     cell, model): values 0-100 as the merge wrote them (suitability inside the range, the governing
#'     `er_score` on range-only cells, ER x suitability for the premultiplied sea turtles). A multi-model
#'     AquaMaps-only taxon keeps duplicate (taxon, cell) rows, as the merge does.}
#'   \item{`model_cell_er`}{`(mdl_key, cell_id, er)` -- the per-cell extinction risk of the
#'     `er_mode = 'cell'` taxa. May be empty, must exist, must cover every scored cell of those taxa.}
#'   \item{`taxon`}{`(ms_merge_key, sp_cat, er_score, er_mode, ...)` plus whatever `taxa_where`
#'     references (`is_valid_usa`, `is_marine`). `er_mode` is one of `taxon`, `premultiplied`, `cell`.}
#'   \item{`taxon_denom`}{`(ms_merge_key, total_global, total_global_strip, source)` -- each species'
#'     total `sum(val / 100 * area_km2)` over its WHOLE modelled range (from [denom_sql()]), on the
#'     same `val` scale as `model_cell`; the `_strip` variant omits the floored cells (`val = 1`) of
#'     premultiplied taxa. Must exist; only the `global` denominator divides by it.}
#'   \item{`cell`}{`(cell_id, area_km2, in_usa)`.}
#'   \item{`zone`, `zone_cell`}{`(zone_seq, fld, val)` and `(zone_seq, cell_id, pct_covered)` with
#'     `pct_covered` in 0-100.}
#'   \item{the nation}{`nation_sql`, a query yielding `(cell_id, pct_covered)`: the cells the national
#'     reference (`share_USA`, `A_USA`) sums over. Default: every `cell.in_usa` cell in full. It is
#'     deliberately NOT a zone row: v8's `USA` pseudo-zone is the union of the subregion polygons and
#'     misses 10,996 coastal and estuarine study-area cells (1.7 % of the cells, 3.4 % of the scored
#'     rows -- the Emperor Goose's whole US range), so a species' national share would not be its US
#'     total. Every scored cell must lie in the nation; [share_run()] asserts it.}
#'   \item{`primprod_cell`}{`(cell_id, npp)` -- optional, for `$primprod`.}
#' }
#'
#' The strings, in the order [share_run()] executes them:
#' \describe{
#'   \item{`taxon_w`}{`CREATE OR REPLACE TEMP TABLE share_taxon`: per scored species of `cats`, the
#'     species weight `w_s`, its US total (`total_usa`, the risk-free value-area over the scoring
#'     surface), its denominator `denom_total` for this `denom`, `f_us = total_usa / total_global`
#'     and the denominator `source`.}
#'   \item{`base`}{`CREATE OR REPLACE TEMP TABLE share_base`: per (zone, species) the two numerators
#'     every denominator divides -- `num = sum(v * A * p / 100)` and `num_w = sum(w * v * A * p / 100)`
#'     (the risk weight applied at cell level so a per-cell ER is honoured) -- and `n_rows`. This is
#'     the one pass over `model_cell x zone_cell`; [share_run()] builds it once and reuses it across
#'     denominators (`share_base_sig` records the arguments it was built with).}
#'   \item{`zone_taxon`}{`CREATE OR REPLACE TEMP TABLE share_zone_taxon`: per (zone, species), the
#'     species' share of its range inside the zone under BOTH per-species denominators
#'     (`share_global`, `share_usa`) and its risk-weighted contribution to the zone total under the
#'     chosen one (`contrib = num_w / D_s / 100`).}
#'   \item{`cat_cell`}{`CREATE OR REPLACE TEMP TABLE share_cat_cell`: per (category, cell), the
#'     risk-weighted sum of the species' cell shares (`share`) and `n_species` (distinct).}
#'   \item{`zone_area`}{`CREATE OR REPLACE TEMP TABLE share_zone_area`: each zone's covered area
#'     `area_km2 = sum(area_km2 * pct_covered / 100)` and, per category, the covered area of the
#'     cells carrying that category (`area_cat`).}
#'   \item{`zone_cat`}{`SELECT`: per (zone, category) the zone total `share`, `n_species`,
#'     `mean_share`, `area_km2`, `coverage`, `pct_nation`, `pct_world`, `v10`; the caller applies the floor.}
#'   \item{`cell_score`}{`SELECT`: per (category, cell) `share`, `n_species` and the cell-level `v10`
#'     (`100 * (share / A_c) / (share_USA / A_USA)`).}
#'   \item{`primprod`}{`SELECT`: per zone the area-weighted mean of `npp` over ALL the zone's cells
#'     (a cell without a value counts 0, as the published `pct_area` down-weight does), its
#'     `coverage`, and `v10 = 100 * mean_z / mean_USA` (`mean_usa` over the nation, same estimator).}
#'   \item{`nation`}{`SELECT`: per category the national reference `share_usa`, `area_usa`, `n_cells`.}
#'   \item{`outside_nation`}{`SELECT`: scored rows / cells outside the nation (must be 0).}
#'   \item{`er_gap`}{`SELECT`: scored cells of `cell`-mode taxa without a per-cell ER (must be 0).}
#'   \item{`er_dup`}{`SELECT`: duplicate `(mdl_key, cell_id)` rows of `model_cell_er` (must be 0: the
#'     LEFT JOIN would double a species' share).}
#' }
#'
#' @param cats character; the scored categories (`taxon.sp_cat` values), default the seven of v8+.
#' @param denom one of `"global"`, `"usa"`, `"pooled"`.
#' @param floor_strip logical; `TRUE` drops the floored cells (`val <= 1`) of premultiplied taxa from
#'   every sum (numerator, US total, coverage, species counts) and takes `total_global_strip` as their
#'   denominator (page §9b, the sensitivity column; the default keeps the surface continuous, plan D4).
#' @param er_sql character; the SQL expression of the per-(taxon, cell) risk weight over aliases `t`
#'   (taxon) and `e` (model_cell_er): the CASE `score_cell_metrics.qmd` uses. Its species-level
#'   counterpart for the percent of the world, `w_s`, is `er_score` for the `taxon` and `cell` modes
#'   and 100 for `premultiplied`.
#' @param val_col character; the value column of `model_cell` (`sdm_val_col(con, "model_cell")`).
#' @param taxa_where character; SQL over alias `t` selecting the scored species.
#' @param nation_sql character; SQL yielding `(cell_id, pct_covered)` for the national reference.
#' @return a named list of SQL strings (see above).
#' @concept score
#' @importFrom glue glue
#' @export
#' @examples
#' sq <- share_sql(denom = "global")
#' names(sq)
share_sql <- function(
  cats        = c("bird", "coral", "fish", "invertebrate", "mammal", "primary_producer", "turtle"),
  denom       = c("global", "usa", "pooled"),
  floor_strip = FALSE,
  er_sql      = "(CASE t.er_mode WHEN 'premultiplied' THEN 100 WHEN 'cell' THEN e.er ELSE t.er_score END)",
  val_col     = "val",
  taxa_where  = "t.is_valid_usa AND t.is_marine",
  nation_sql  = "SELECT cell_id, 100.0 AS pct_covered FROM cell WHERE in_usa") {

  denom <- match.arg(denom)
  stopifnot(
    is.character(cats), length(cats) >= 1L, all(nzchar(cats)), !anyDuplicated(cats),
    is.logical(floor_strip), length(floor_strip) == 1L, !is.na(floor_strip),
    is.character(er_sql), length(er_sql) == 1L, nzchar(er_sql),
    is.character(val_col), length(val_col) == 1L, nzchar(val_col),
    is.character(taxa_where), length(taxa_where) == 1L, nzchar(taxa_where),
    is.character(nation_sql), length(nation_sql) == 1L, nzchar(nation_sql))

  cats_sql <- paste(vapply(cats, .sql_str, ""), collapse = ", ")
  v        <- glue::glue("mc.{val_col}")
  # the floor strip REMOVES the floored cells (val = 1) of the premultiplied taxa from every sum: a
  # floored cell is `greatest(1, round(er * suit / 100))` where suit rounded to 0, i.e. the range
  # fill, not habitat (page §9a step 1). A removed cell counts neither for coverage nor for n_species.
  keep     <- if (floor_strip) glue::glue("NOT (t.er_mode = 'premultiplied' AND {v} <= 1)") else "TRUE"
  gl_total <- if (floor_strip) "d.total_global_strip" else "d.total_global"
  # the species-level weight the percent-of-world uses (w_s)
  w_s      <- "(CASE t.er_mode WHEN 'premultiplied' THEN 100 ELSE t.er_score END)"
  denom_expr <- switch(denom, global = "st.denom_total", usa = "st.total_usa", pooled = "1.0")
  scored   <- glue::glue("{taxa_where} AND t.sp_cat IN ({cats_sql})")

  list(
    taxon_w = glue::glue("
      CREATE OR REPLACE TEMP TABLE share_taxon AS
      WITH us AS (
        SELECT mc.mdl_key AS ms_merge_key,
               SUM({v} / 100.0 * c.area_km2) AS total_usa
        FROM model_cell mc
        JOIN taxon t ON t.ms_merge_key = mc.mdl_key
        JOIN cell  c ON c.cell_id = mc.cell_id
        WHERE {scored} AND {keep}
        GROUP BY mc.mdl_key)
      SELECT t.ms_merge_key, t.sp_cat, t.er_mode,
             CAST({w_s} AS DOUBLE)                               AS w_s,
             us.total_usa,
             {gl_total}                                          AS total_global,
             d.source,
             CASE '{denom}' WHEN 'global' THEN {gl_total} WHEN 'usa' THEN us.total_usa ELSE 1.0 END AS denom_total,
             CASE WHEN {gl_total} > 0 THEN us.total_usa / {gl_total} END AS f_us
      FROM taxon t
      JOIN us ON us.ms_merge_key = t.ms_merge_key
      LEFT JOIN taxon_denom d ON d.ms_merge_key = t.ms_merge_key
      WHERE {scored}", .trim = TRUE),

    base = glue::glue("
      CREATE OR REPLACE TEMP TABLE share_base AS
      SELECT zc.zone_seq, mc.mdl_key AS ms_merge_key, t.sp_cat,
             SUM({v} / 100.0 * c.area_km2 * zc.pct_covered / 100.0)            AS num,
             SUM({er_sql} * {v} / 100.0 * c.area_km2 * zc.pct_covered / 100.0) AS num_w,
             COUNT(*)                                                           AS n_rows
      FROM model_cell mc
      JOIN taxon t        ON t.ms_merge_key = mc.mdl_key
      JOIN cell c         ON c.cell_id = mc.cell_id
      JOIN zone_cell zc   ON zc.cell_id = mc.cell_id
      LEFT JOIN model_cell_er e ON e.mdl_key = mc.mdl_key AND e.cell_id = mc.cell_id
      WHERE {scored} AND {keep}
      GROUP BY zc.zone_seq, mc.mdl_key, t.sp_cat", .trim = TRUE),

    zone_taxon = glue::glue("
      CREATE OR REPLACE TEMP TABLE share_zone_taxon AS
      SELECT b.zone_seq, b.ms_merge_key, b.sp_cat,
             b.num / st.total_global              AS share_global,
             b.num / st.total_usa                 AS share_usa,
             b.num_w / {denom_expr} / 100.0       AS contrib
      FROM share_base b
      JOIN share_taxon st ON st.ms_merge_key = b.ms_merge_key", .trim = TRUE),

    cat_cell = glue::glue("
      CREATE OR REPLACE TEMP TABLE share_cat_cell AS
      SELECT st.sp_cat, mc.cell_id,
             SUM({er_sql} * ({v} / 100.0 * c.area_km2) / {denom_expr}) / 100.0 AS share,
             COUNT(DISTINCT mc.mdl_key) AS n_species
      FROM model_cell mc
      JOIN share_taxon st ON st.ms_merge_key = mc.mdl_key
      JOIN taxon t        ON t.ms_merge_key = mc.mdl_key
      JOIN cell c         ON c.cell_id = mc.cell_id
      LEFT JOIN model_cell_er e ON e.mdl_key = mc.mdl_key AND e.cell_id = mc.cell_id
      WHERE {keep}
      GROUP BY st.sp_cat, mc.cell_id", .trim = TRUE),

    zone_area = glue::glue("
      CREATE OR REPLACE TEMP TABLE share_zone_area AS
      WITH za AS (
        SELECT zc.zone_seq, SUM(c.area_km2 * zc.pct_covered / 100.0) AS area_km2
        FROM zone_cell zc JOIN cell c ON c.cell_id = zc.cell_id
        GROUP BY zc.zone_seq),
      zcat AS (
        SELECT zc.zone_seq, cc.sp_cat, SUM(c.area_km2 * zc.pct_covered / 100.0) AS area_cat
        FROM zone_cell zc
        JOIN share_cat_cell cc ON cc.cell_id = zc.cell_id
        JOIN cell c            ON c.cell_id = zc.cell_id
        GROUP BY zc.zone_seq, cc.sp_cat)
      SELECT za.zone_seq, zcat.sp_cat, za.area_km2, zcat.area_cat
      FROM za JOIN zcat ON zcat.zone_seq = za.zone_seq", .trim = TRUE),

    zone_cat = glue::glue("
      WITH zs AS (
        SELECT zone_seq, sp_cat, SUM(contrib) AS share, COUNT(*) AS n_species
        FROM share_zone_taxon GROUP BY zone_seq, sp_cat),
      sw AS (
        SELECT sp_cat, SUM(w_s) / 100.0 AS sum_w FROM share_taxon GROUP BY sp_cat),
      nation AS ({nation_sql}),
      na AS (
        SELECT SUM(c.area_km2 * n.pct_covered / 100.0) AS area_usa
        FROM nation n JOIN cell c ON c.cell_id = n.cell_id),
      usa AS (
        SELECT cc.sp_cat, SUM(cc.share * n.pct_covered / 100.0) AS share_usa, na.area_usa
        FROM share_cat_cell cc JOIN nation n ON n.cell_id = cc.cell_id, na
        GROUP BY cc.sp_cat, na.area_usa)
      SELECT zs.zone_seq, zs.sp_cat, zs.share, zs.n_species,
             zs.share / zs.n_species                                  AS mean_share,
             za.area_km2,
             za.area_cat / za.area_km2                                AS coverage,
             100.0 * zs.share / usa.share_usa                         AS pct_nation,
             CASE WHEN '{denom}' = 'global' THEN 100.0 * zs.share / sw.sum_w END AS pct_world,
             100.0 * (zs.share / usa.share_usa) / (za.area_km2 / usa.area_usa) AS v10
      FROM zs
      JOIN share_zone_area za ON za.zone_seq = zs.zone_seq AND za.sp_cat = zs.sp_cat
      JOIN sw  ON sw.sp_cat = zs.sp_cat
      LEFT JOIN usa ON usa.sp_cat = zs.sp_cat
      ORDER BY zs.zone_seq, zs.sp_cat", .trim = TRUE),

    cell_score = glue::glue("
      WITH nation AS ({nation_sql}),
      na AS (
        SELECT SUM(c.area_km2 * n.pct_covered / 100.0) AS area_usa
        FROM nation n JOIN cell c ON c.cell_id = n.cell_id),
      usa AS (
        SELECT cc.sp_cat, SUM(cc.share * n.pct_covered / 100.0) AS share_usa, na.area_usa
        FROM share_cat_cell cc JOIN nation n ON n.cell_id = cc.cell_id, na
        GROUP BY cc.sp_cat, na.area_usa)
      SELECT cc.sp_cat, cc.cell_id, cc.share, cc.n_species,
             100.0 * (cc.share / c.area_km2) / (usa.share_usa / usa.area_usa) AS v10
      FROM share_cat_cell cc
      JOIN cell c ON c.cell_id = cc.cell_id
      JOIN usa    ON usa.sp_cat = cc.sp_cat
      ORDER BY cc.sp_cat, cc.cell_id", .trim = TRUE),

    primprod = glue::glue("
      WITH z AS (
        SELECT zc.zone_seq,
               SUM(coalesce(p.npp, 0) * c.area_km2 * zc.pct_covered / 100.0) /
                 SUM(c.area_km2 * zc.pct_covered / 100.0)                                         AS mean_npp,
               SUM(CASE WHEN p.npp IS NOT NULL THEN c.area_km2 * zc.pct_covered / 100.0 END) /
                 SUM(c.area_km2 * zc.pct_covered / 100.0)                                         AS coverage,
               SUM(c.area_km2 * zc.pct_covered / 100.0)                                           AS area_km2
        FROM zone_cell zc
        JOIN cell c ON c.cell_id = zc.cell_id
        LEFT JOIN primprod_cell p ON p.cell_id = zc.cell_id
        GROUP BY zc.zone_seq),
      nation AS ({nation_sql}),
      usa AS (
        SELECT SUM(coalesce(p.npp, 0) * c.area_km2 * n.pct_covered / 100.0) /
                 SUM(c.area_km2 * n.pct_covered / 100.0) AS mean_usa
        FROM nation n JOIN cell c ON c.cell_id = n.cell_id
        LEFT JOIN primprod_cell p ON p.cell_id = n.cell_id)
      SELECT z.zone_seq, z.mean_npp, coalesce(z.coverage, 0) AS coverage, z.area_km2,
             100.0 * z.mean_npp / usa.mean_usa AS v10, usa.mean_usa
      FROM z, usa
      ORDER BY z.zone_seq", .trim = TRUE),

    nation = glue::glue("
      WITH nation AS ({nation_sql}),
      na AS (
        SELECT SUM(c.area_km2 * n.pct_covered / 100.0) AS area_usa, COUNT(*) AS n_cells
        FROM nation n JOIN cell c ON c.cell_id = n.cell_id)
      SELECT cc.sp_cat, SUM(cc.share * n.pct_covered / 100.0) AS share_usa, na.area_usa, na.n_cells
      FROM share_cat_cell cc JOIN nation n ON n.cell_id = cc.cell_id, na
      GROUP BY cc.sp_cat, na.area_usa, na.n_cells
      ORDER BY cc.sp_cat", .trim = TRUE),

    outside_nation = glue::glue("
      WITH nation AS ({nation_sql})
      SELECT COUNT(*) AS n_rows, COUNT(DISTINCT mc.cell_id) AS n_cells
      FROM model_cell mc JOIN share_taxon st ON st.ms_merge_key = mc.mdl_key
      WHERE NOT EXISTS (SELECT 1 FROM nation n WHERE n.cell_id = mc.cell_id)", .trim = TRUE),

    er_gap = glue::glue("
      SELECT COUNT(*) AS n
      FROM model_cell mc JOIN share_taxon st ON st.ms_merge_key = mc.mdl_key
      LEFT JOIN model_cell_er e ON e.mdl_key = mc.mdl_key AND e.cell_id = mc.cell_id
      WHERE st.er_mode = 'cell' AND e.er IS NULL", .trim = TRUE),

    er_dup = "SELECT COUNT(*) - COUNT(DISTINCT (mdl_key, cell_id)) AS n FROM model_cell_er")
}

#' Run the v10 share family on a scoring connection
#'
#' Executes [share_sql()] in order and returns the results. Writes only `share_*` temporary tables:
#' `share_base` (the one pass over `model_cell x zone_cell`) is kept and reused by a later call with
#' the same `cats`, `floor_strip`, `er_sql`, `val_col` and `taxa_where` (its signature is stored in
#' `share_base_sig`), so the three denominators cost one heavy pass, not three; the other
#' temporaries are dropped before returning.
#'
#' Hard stops (the rule's own assertions): under `global`, every scored species must have a positive
#' denominator from an accepted source (`merged_global`, `aquamaps`, `fallback:*`; a
#' `national_only_unresolved`, `cell_er_unsupported` or `none` row is refused -- curate
#' `data/us_share_fallback.csv` or fix the totals) and `f_us <= 1`; every scored species must have a
#' positive US total; every scored cell must lie in the national reference; every scored cell of a
#' `cell`-mode taxon must have its per-cell ER and `model_cell_er` no duplicate row; a species may
#' appear once in `share_taxon`. Under the `global` denominator the percent of the world divides by
#' `er_score` for a `cell`-mode species while its integrated contribution is its suitability-weighted
#' mean ER (<= er_score), so that companion is an upper-bound convention for such taxa; none exists
#' in v10, and `denom_sql()` flags them until v11 settles their global ER.
#'
#' @inheritParams share_sql
#' @param con a DBI connection holding the relations [share_sql()] documents.
#' @param kappa numeric; the reportability floor on `coverage` (default 0.05), applied with
#'   [coverage_floor()]'s tolerance.
#' @param primprod logical; also compute `$primprod` from `primprod_cell` (default: when that table
#'   exists).
#' @param reuse_base logical; reuse an existing `share_base` built with the same arguments (default
#'   `TRUE`); `FALSE` rebuilds it.
#' @return a list: `taxon` (the `share_taxon` table), `nation` (per category: `share_usa`, `area_usa`,
#'   `n_cells` of the national reference), `zone` (per zone x category: `share`, `n_species`,
#'   `mean_share`, `area_km2`, `coverage`, `reportable`, `pct_nation`, `pct_world`, `v10` -- `NA`
#'   where not reportable, the row kept so the caller can publish coverage and counts), `zone_taxon`
#'   (per zone x species), `cell` (per category x cell: `share`, `n_species`, `v10`), `primprod`
#'   (per zone, or `NULL`), `composite` (per zone: the equal-weight mean of the reportable
#'   components' `v10`, `n_components`), and `denom`, `floor_strip`, `kappa`.
#' @concept score
#' @importFrom DBI dbExecute dbGetQuery dbListTables
#' @export
share_run <- function(
  con,
  cats        = c("bird", "coral", "fish", "invertebrate", "mammal", "primary_producer", "turtle"),
  denom       = c("global", "usa", "pooled"),
  kappa       = 0.05,
  floor_strip = FALSE,
  er_sql      = "(CASE t.er_mode WHEN 'premultiplied' THEN 100 WHEN 'cell' THEN e.er ELSE t.er_score END)",
  val_col     = "val",
  taxa_where  = "t.is_valid_usa AND t.is_marine",
  nation_sql  = "SELECT cell_id, 100.0 AS pct_covered FROM cell WHERE in_usa",
  primprod    = "primprod_cell" %in% DBI::dbListTables(con),
  reuse_base  = TRUE) {

  denom <- match.arg(denom)
  stopifnot(
    is.numeric(kappa), length(kappa) == 1L, !is.na(kappa), kappa >= 0, kappa <= 1,
    is.logical(primprod), length(primprod) == 1L,
    is.logical(reuse_base), length(reuse_base) == 1L)
  sq <- share_sql(cats = cats, denom = denom, floor_strip = floor_strip, er_sql = er_sql,
                  val_col = val_col, taxa_where = taxa_where, nation_sql = nation_sql)
  head5 <- function(x) paste(utils::head(x, 5), collapse = ", ")

  n_nation <- DBI::dbGetQuery(con, glue::glue("SELECT count(*) n FROM ({nation_sql})"))$n
  if (n_nation < 1L) stop("the national reference (nation_sql) selects no cell", call. = FALSE)

  # the working temporaries go even when a hard stop fires below; share_base is kept for reuse
  on.exit(for (t in c("share_cat_cell", "share_zone_area", "share_zone_taxon", "share_taxon"))
    DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", t)), add = TRUE)
  DBI::dbExecute(con, sq$taxon_w)
  taxon <- DBI::dbGetQuery(con, "SELECT * FROM share_taxon ORDER BY sp_cat, ms_merge_key")
  if (anyDuplicated(taxon$ms_merge_key))
    stop("a species appears more than once in share_taxon (duplicate taxon_denom / taxon rows): ",
         head5(unique(taxon$ms_merge_key[duplicated(taxon$ms_merge_key)])), call. = FALSE)
  if (any(is.na(taxon$total_usa) | taxon$total_usa <= 0))
    stop("a scored species has no positive US total; the scoring surface and the species set disagree", call. = FALSE)
  if (denom == "global") {
    ok_src <- !is.na(taxon$source) & (taxon$source %in% c("merged_global", "aquamaps") | startsWith(taxon$source, "fallback:"))
    if (!all(ok_src))
      stop("every scored species needs a global denominator from an accepted source (merged_global, aquamaps, fallback:*); refused: ",
           head5(paste0(taxon$ms_merge_key[!ok_src], " [", taxon$source[!ok_src], "]")), call. = FALSE)
    bad <- is.na(taxon$total_global) | taxon$total_global <= 0
    if (any(bad)) stop("every scored species needs a positive global denominator; missing for: ", head5(taxon$ms_merge_key[bad]), call. = FALSE)
    if (any(taxon$f_us > 1 + 1e-9))
      stop("a species' US total exceeds its global total (f_us > 1): ", head5(taxon$ms_merge_key[taxon$f_us > 1 + 1e-9]), call. = FALSE)
  }
  gap <- DBI::dbGetQuery(con, sq$er_gap)$n
  if (gap > 0) stop(gap, " scored cell(s) of er_mode = 'cell' taxa have no per-cell ER (model_cell_er)", call. = FALSE)
  dup <- DBI::dbGetQuery(con, sq$er_dup)$n
  if (dup > 0) stop(dup, " duplicate (mdl_key, cell_id) row(s) in model_cell_er would double a species' share", call. = FALSE)
  out <- DBI::dbGetQuery(con, sq$outside_nation)
  if (out$n_rows > 0)
    stop(out$n_cells, " scored cell(s) (", out$n_rows, " rows) lie outside the national reference; ",
         "nation_sql must cover the whole scoring surface", call. = FALSE)

  # the heavy pass, built once per argument set
  sig <- paste(c(sort(cats), floor_strip, er_sql, val_col, taxa_where), collapse = "\n")
  have <- all(c("share_base", "share_base_sig") %in% DBI::dbListTables(con)) &&
    identical(DBI::dbGetQuery(con, "SELECT sig FROM share_base_sig")$sig, sig)
  if (!reuse_base || !have) {
    DBI::dbExecute(con, sq$base)
    DBI::dbExecute(con, "CREATE OR REPLACE TEMP TABLE share_base_sig (sig VARCHAR)")
    DBI::dbExecute(con, "INSERT INTO share_base_sig VALUES (?)", params = list(sig))
  }
  DBI::dbExecute(con, sq$zone_taxon)
  DBI::dbExecute(con, sq$cat_cell)
  DBI::dbExecute(con, sq$zone_area)

  nation <- DBI::dbGetQuery(con, sq$nation)
  zone <- DBI::dbGetQuery(con, sq$zone_cat)
  zone <- coverage_floor(zone, kappa = kappa)
  zone$v10[!zone$reportable] <- NA_real_
  zone_taxon <- DBI::dbGetQuery(con, "SELECT * FROM share_zone_taxon ORDER BY zone_seq, sp_cat, ms_merge_key")
  cell <- DBI::dbGetQuery(con, sq$cell_score)

  pp <- NULL
  if (primprod) {
    pp <- DBI::dbGetQuery(con, sq$primprod)
    pp <- coverage_floor(pp, kappa = kappa)
    pp$v10[!pp$reportable] <- NA_real_
  }

  comp_parts <- rbind(
    data.frame(zone_seq = zone$zone_seq, component = zone$sp_cat, v10 = zone$v10, stringsAsFactors = FALSE),
    if (!is.null(pp)) data.frame(zone_seq = pp$zone_seq, component = "primprod", v10 = pp$v10, stringsAsFactors = FALSE))
  comp_parts <- comp_parts[!is.na(comp_parts$v10), , drop = FALSE]
  composite <- if (nrow(comp_parts)) {
    sp <- split(comp_parts$v10, comp_parts$zone_seq)
    data.frame(zone_seq = as.integer(names(sp)), score = vapply(sp, mean, 0), n_components = vapply(sp, length, 0L),
               row.names = NULL, stringsAsFactors = FALSE)
  } else data.frame(zone_seq = integer(), score = numeric(), n_components = integer())

  list(taxon = taxon, zone = zone, zone_taxon = zone_taxon, cell = cell, primprod = pp, nation = nation,
       composite = composite, denom = denom, floor_strip = floor_strip, kappa = kappa)
}

#' SQL for each species' global denominator (its value-area total over its whole modelled range)
#'
#' The share needs, per species, `D_s = sum over range(s) of v * A` on the same `val` scale as the
#' US scoring surface. Three sources, in order of precedence (page §8 and §9b; the review page's
#' `scripts/v10_review/02-05` recipe):
#' \enumerate{
#'   \item a curated fallback fraction `f` (`fallback`): `D_s = total_usa / f`, `source =
#'     'fallback:<method>'` -- it takes precedence because a national-only taxon's "global"
#'     merged surface IS its US range (FWS-range-only birds, fishes, shrimps, sea-otter subspecies);
#'   \item the merged global surface (`mc_global`): the whole-range `max(er, suitability-at-range)`
#'     the merge wrote for every range-constrained taxon, `source = 'merged_global'`;
#'   \item AquaMaps-only taxa (no merged global surface, since they reuse the AquaMaps COGs):
#'     AquaMaps outside the US (`mc_am` where `NOT cell.in_usa`, every model of the taxon, no
#'     dedup -- as the US am-only branch keeps duplicates) plus the merged US surface inside,
#'     `source = 'aquamaps'`; a taxon with AquaMaps cells only inside the US gets `D_s = total_usa`
#'     (`f_us = 1`, an EEZ endemic as modelled).
#' }
#' A taxon whose datasets are ALL national (`national_ds`) and that has no fallback row gets
#' `source = 'national_only_unresolved'` with its US-range total as a placeholder, so the caller
#' can refuse or curate it (`data/us_share_fallback.csv`) rather than score a species at share 1.
#' An `er_mode = 'cell'` taxon is flagged `cell_er_unsupported` until its treatment is settled
#' (see the header of `R/share.R`; no v10 taxon is in that mode).
#'
#' **The whole range includes land.** The merged global surfaces carry a bird's entire range (the
#' ingests apply no land mask, by design), while `cell` holds ocean cells only (17.1 M of the
#' 25.9 M `global05` cells); on v8 one bird has 1.66 M global cells of which 1.5 % are in `cell`.
#' Joining the surfaces to `cell` would silently measure a bird's "range" as its marine fraction
#' and inflate `f_us` up to ~60x. So an ocean cell takes its own `cell.area_km2` (terra's
#' ellipsoidal area, exactly what the US numerator uses) and a cell absent from `cell` takes the
#' area of its latitude row from `row_area` (one value per row from that row's ocean cells; every
#' cell of a row has the same area on a regular lat-lon grid) -- no sphere-vs-ellipsoid mismatch. A
#' land cell in a row with no ocean cell at all contributes nothing (counted in `n_cells_no_area`). `nc` is the grid's column count
#' (`grid_spec_for(grid_id)$nc`); `cell_id` is 1-based row-major.
#'
#' Both totals come in a plain and a `_strip` variant: the strip omits the floored cells
#' (`val <= 1`) of `er_mode = 'premultiplied'` taxa, for [share_sql()]'s `floor_strip`.
#'
#' Relations the caller materializes: `model_cell (mdl_key, cell_id, <val_col>)` the US scoring
#' surface; `mc_global (mdl_key, cell_id, <val_col>)` the merged global surface (a view over
#' `dist_merged_global`); `mc_am (mdl_key, cell_id, <val_col>)` the AquaMaps surfaces (a view over
#' `dist/dataset=am`); `taxon_model (mdl_key, ms_merge_key, ds_key)`; `taxon (ms_merge_key, er_mode,
#' ...)`; `cell (cell_id, area_km2, in_usa)`; `fallback (ms_merge_key, fraction, method)` with one
#' row per species.
#'
#' @param val_col character; the value column of the three surfaces.
#' @param nc integer; the grid's number of columns (7200 for `global05`).
#' @param national_ds character; dataset keys that never carry a global range.
#' @param suit_ds character; the suitability dataset key(s) feeding the am-only branch.
#' @return SQL (`SELECT`) yielding `taxon_denom (ms_merge_key, total_usa, total_usa_strip,
#'   total_global, total_global_strip, n_cells_global, n_cells_no_area, source)` for every taxon with
#'   a US scoring surface.
#' @concept score
#' @importFrom glue glue
#' @export
denom_sql <- function(
  val_col     = "val",
  nc          = 7200L,
  national_ds = c("rng_fws", "ca_nmfs", "ch_fws", "ch_nmfs", "dps_nmfs", "ax"),
  suit_ds     = "am") {

  stopifnot(
    is.character(val_col), length(val_col) == 1L, nzchar(val_col),
    is.numeric(nc), length(nc) == 1L, !is.na(nc), nc >= 1,
    is.character(national_ds), length(national_ds) >= 1L,
    is.character(suit_ds), length(suit_ds) >= 1L)
  nc       <- as.integer(nc)
  nat_sql  <- paste(vapply(national_ds, .sql_str, ""), collapse = ", ")
  suit_sql <- paste(vapply(suit_ds, .sql_str, ""), collapse = ", ")
  strip    <- function(v) glue::glue("(CASE WHEN t.er_mode = 'premultiplied' AND {v} <= 1 THEN 0 ELSE {v} END)")
  row_of   <- function(id) glue::glue("CAST(floor(({id} - 1) / {nc}) AS INTEGER)")

  glue::glue("
    WITH row_area AS (
      SELECT {row_of('cell_id')} AS row, any_value(area_km2) AS area_km2
      FROM cell GROUP BY 1),
    us AS (
      SELECT mc.mdl_key AS ms_merge_key, t.er_mode,
             SUM(mc.{val_col} / 100.0 * c.area_km2)                        AS total_usa,
             SUM({strip(paste0('mc.', val_col))} / 100.0 * c.area_km2)     AS total_usa_strip
      FROM model_cell mc JOIN cell c ON c.cell_id = mc.cell_id
      JOIN taxon t ON t.ms_merge_key = mc.mdl_key
      GROUP BY mc.mdl_key, t.er_mode),
    gl AS (
      SELECT g.mdl_key AS ms_merge_key,
             SUM(g.{val_col} / 100.0 * coalesce(c.area_km2, ra.row_km2))                    AS total_global,
             SUM({strip(paste0('g.', val_col))} / 100.0 * coalesce(c.area_km2, ra.row_km2)) AS total_global_strip,
             COUNT(*)                                                                      AS n_cells_global,
             COUNT(*) FILTER (WHERE c.area_km2 IS NULL AND ra.row_km2 IS NULL)              AS n_cells_no_area
      FROM mc_global g
      JOIN taxon t ON t.ms_merge_key = g.mdl_key
      LEFT JOIN cell c ON c.cell_id = g.cell_id
      LEFT JOIN (SELECT row, area_km2 AS row_km2 FROM row_area) ra ON ra.row = {row_of('g.cell_id')}
      GROUP BY g.mdl_key),
    am_out AS (
      SELECT tm.ms_merge_key,
             SUM(CASE WHEN NOT c.in_usa THEN a.{val_col} / 100.0 * c.area_km2 ELSE 0 END)                    AS am_outside,
             SUM(CASE WHEN NOT c.in_usa THEN {strip(paste0('a.', val_col))} / 100.0 * c.area_km2 ELSE 0 END) AS am_outside_strip
      FROM mc_am a
      JOIN taxon_model tm ON tm.mdl_key = a.mdl_key AND tm.ds_key IN ({suit_sql})
      JOIN taxon t ON t.ms_merge_key = tm.ms_merge_key
      JOIN cell c ON c.cell_id = a.cell_id
      GROUP BY tm.ms_merge_key),
    nat AS (
      SELECT ms_merge_key, bool_and(ds_key IN ({nat_sql})) AS national_only
      FROM taxon_model GROUP BY ms_merge_key)
    SELECT us.ms_merge_key, us.total_usa, us.total_usa_strip,
      CASE WHEN fb.fraction IS NOT NULL THEN us.total_usa / fb.fraction
           WHEN gl.total_global IS NOT NULL THEN gl.total_global
           WHEN am_out.am_outside IS NOT NULL THEN am_out.am_outside + us.total_usa
           END AS total_global,
      CASE WHEN fb.fraction IS NOT NULL THEN us.total_usa_strip / fb.fraction
           WHEN gl.total_global IS NOT NULL THEN gl.total_global_strip
           WHEN am_out.am_outside IS NOT NULL THEN am_out.am_outside_strip + us.total_usa_strip
           END AS total_global_strip,
      gl.n_cells_global, coalesce(gl.n_cells_no_area, 0) AS n_cells_no_area,
      CASE WHEN us.er_mode = 'cell' THEN 'cell_er_unsupported'
           WHEN fb.fraction IS NOT NULL THEN 'fallback:' || fb.method
           WHEN coalesce(nat.national_only, FALSE) THEN 'national_only_unresolved'
           WHEN gl.total_global IS NOT NULL THEN 'merged_global'
           WHEN am_out.am_outside IS NOT NULL THEN 'aquamaps'
           ELSE 'none' END AS source
    FROM us
    LEFT JOIN gl     ON gl.ms_merge_key = us.ms_merge_key
    LEFT JOIN am_out ON am_out.ms_merge_key = us.ms_merge_key
    LEFT JOIN nat    ON nat.ms_merge_key = us.ms_merge_key
    LEFT JOIN fallback fb ON fb.ms_merge_key = us.ms_merge_key
    ORDER BY us.ms_merge_key", .trim = TRUE)
}
