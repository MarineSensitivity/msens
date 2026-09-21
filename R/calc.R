#' Cell-ID SpatRaster (the **v7** grid)
#'
#' `derived/r_bio-oracle_planarea.tif` — a **regional** raster on **0-360**
#' longitudes whose pixel values are **v7** `cell_id`s.
#'
#' It is NOT version-neutral, despite the generic name. v8 uses a different grid
#' entirely: global, `[-180,180]`, `cell_id` 1..24,293,128. Handing this raster
#' to [cells_in_polygon()] against a **v8** database therefore yields ids that do
#' exist in v8 but denote **completely different places** — a polygon off Santa
#' Barbara resolved to cells in the Arctic, so `species_for_cells()` returned
#' zero species with no error at all. Prefer passing the DB connection to
#' [cells_in_polygon()], which picks the right grid for the version it is
#' actually looking at.
#'
#' @return a [`terra::SpatRaster`] with a `cell_id` layer
#' @importFrom terra rast
#' @importFrom glue glue
#' @export
#' @concept calc
cell_id_raster <- function() {
  dir_data <- switch(
    Sys.info()[["sysname"]],
    "Darwin" = "~/My Drive/projects/msens/data",
    "Linux"  = "/share/data")
  terra::rast(file.path(dir_data, "derived/r_bio-oracle_planarea.tif"))
}

#' Cells intersecting a polygon
#'
#' Returns `cell_id` + `pct_covered` (0-100) for the cells a polygon overlaps.
#' `pct_covered` is the fraction of each cell inside the polygon, and it is not
#' cosmetic — [scores_for_cells()] and [species_for_cells()] weight by it, so
#' partially covered edge cells count proportionally.
#'
#' **Pass a DB connection.** Then the grid is read from the database being
#' queried ([grid_for_con()]) and cannot disagree with it. Both generations take
#' the SAME path: [cells_in_polygon_grid()] computes the cells from the grid
#' definition alone, and the result is restricted to the `cell_id`s the release
#' actually holds. No raster is opened on any version — the v7 branch used to read
#' the 0-360 cell-id raster and returned `terra`'s coverage fractions, which
#' differed from the v8 path by a mean 0.66 pp on edge cells for no reason anyone
#' could act on.
#'
#' Passing a [`terra::SpatRaster`] directly still works and still uses
#' `terra::extract()`: a cell-id COG is a lookup IMAGE whose pixel values are ids
#' from a frame that may not be the grid's, so arithmetic cannot be applied to it.
#' Nothing can verify such a raster matches the database — see [cell_id_raster()]
#' for how that failed silently on v8.
#'
#' @param poly an sf polygon (assumed or transformable to EPSG:4326)
#' @param src a DBI connection (**preferred**), or a single-layer
#'   [`terra::SpatRaster`] of integer cell ids
#' @param res ignored, kept for back-compatibility; the resolution now comes from
#'   the grid registry, which is the only place it was ever right
#' @return a tibble with columns `cell_id` (integer) and `pct_covered` (0-100)
#' @importFrom sf st_transform st_shift_longitude st_geometry st_union st_bbox
#'   st_set_crs st_sfc st_polygon st_intersects st_intersection st_area
#' @importFrom terra rasterize vect values extract
#' @importFrom tibble tibble
#' @importFrom DBI dbGetQuery
#' @importFrom methods is
#' @importFrom stats aggregate
#' @export
#' @concept calc
cells_in_polygon <- function(poly, src, res = 0.05) {
  # methods::is(), not inherits(): a duckdb_connection is an S4 object, and S4
  # superclasses are not reliably visible to inherits()
  if (methods::is(src, "DBIConnection"))
    return(.cells_in_polygon_db(poly, src))
  .cells_in_polygon_raster(poly, src)
}

# v7 path: read the cell ids under the polygon from the 0-360 cell-id raster.
#
# terra::extract() reads only the polygon's WINDOW. The previous implementation
# rasterized the polygon across the full extent and then pulled the entire raster
# into memory with terra::values() — for a 2006x3103 grid that is 6.2M cells read
# to find ~450, and it dominated the whole v7 report: measured on the server,
# 35.16 s vs 0.12 s for the same polygon (295x), yielding an IDENTICAL cell_id
# set. `exact = TRUE` returns each cell's covered `fraction`, so pct_covered
# keeps its meaning (it weights area_km2/avg_suit downstream); it differs from
# the old cover= values by a mean 0.66pp on edge cells, being the more precise
# of the two.
.cells_in_polygon_raster <- function(poly, r_cell_id) {
  poly_t <- poly |>
    sf::st_transform(4326) |>
    sf::st_shift_longitude() # [-180,180] -> [0,360]
  e <- terra::extract(r_cell_id, terra::vect(poly_t), exact = TRUE)
  ids <- e[[2]]
  keep <- !is.na(ids) & !is.na(e$fraction) & e$fraction > 0
  if (!any(keep))
    return(tibble::tibble(cell_id = integer(), pct_covered = numeric()))
  # a multi-feature polygon can report the same cell once per feature; sum the
  # fractions (capped at 1) so overlapping parts do not double-count
  agg <- stats::aggregate(
    list(fraction = as.numeric(e$fraction[keep])),
    by = list(cell_id = as.integer(ids[keep])), FUN = sum)
  tibble::tibble(
    cell_id     = agg$cell_id,
    pct_covered = round(pmin(agg$fraction, 1) * 100))
}

# does this database's `cell` table carry lon/lat? (v8 yes, v7 no)
.cell_has_lonlat <- function(con) {
  cols <- tryCatch(
    names(DBI::dbGetQuery(con, "SELECT * FROM cell LIMIT 0")),
    error = function(e) character())
  all(c("lon", "lat") %in% cols)
}

# Both database generations now run ONE rule: [cells_in_polygon_grid()] on the grid
# the connection reports, then restricted to the cell ids the release actually has.
#
# The restriction is not cosmetic. Grid arithmetic answers "which squares does this
# polygon overlap", which includes land, foreign waters and (on v7) the cells the
# regional grid defines but the release never scored. Intersecting with `cell` keeps
# exactly the rows the previous bbox-select returned, while removing the two things
# that made the old pair of implementations disagree: the v7 raster read (grid ids
# from a file whose frame nothing verified) and `exactextractr`-style coverage
# fractions on one side only.
.cells_in_polygon_db <- function(poly, con) {
  empty <- tibble::tibble(cell_id = integer(), pct_covered = numeric())
  d <- cells_in_polygon_grid(poly, grid_for_con(con))
  if (!nrow(d)) return(empty)
  have <- DBI::dbGetQuery(con, sprintf(
    "SELECT cell_id FROM cell WHERE cell_id IN (%s)",
    paste(as.integer(d$cell_id), collapse = ", ")))$cell_id
  d <- d[d$cell_id %in% as.integer(have), , drop = FALSE]
  if (!nrow(d)) return(empty)
  tibble::tibble(cell_id = as.integer(d$cell_id), pct_covered = as.numeric(d$pct_covered))
}

#' Cells belonging to a Program Area zone
#'
#' Fast lookup of the cells making up a Program Area by reading directly from the
#' `zone` / `zone_cell` tables. Returns the same shape (`cell_id`, `pct_covered`)
#' as [cells_in_polygon()] so downstream helpers consume the two interchangeably.
#'
#' **`pct_covered` is the stored coverage, not 100.** `zone_cell` membership is
#' *not* binary: it holds the `exactextractr` coverage fraction x 100, and 74,938
#' of 2,241,876 rows are partial. This function used to overwrite all of them with
#' `100L`, so a single report weighted its Program-Area *scores* by coverage (via
#' the precomputed `zone_metric`) while weighting the *species table* of the same
#' area uniformly — the edge cells of every coastal area counted for more species
#' than they contributed area. `test-calc.R` keeps that as a named regression.
#'
#' The zone key column is resolved with [sdm_val_col()]: v1-v7 spell it `value`,
#' v8+ `val`, and a served v8 view carries both. Hardcoding `value` made this
#' function work on every *served* release and fail on a v8/v9 *source* database.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param pra_key Program Area key (e.g. "CGM")
#' @return tibble(cell_id integer, pct_covered numeric 1-100)
#' @importFrom DBI dbGetQuery dbQuoteString
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @export
#' @concept calc
cells_in_pra <- function(con, pra_key) {
  vc <- sdm_val_col(con, "zone")
  d  <- DBI::dbGetQuery(con, glue::glue(
    "SELECT zc.cell_id, zc.pct_covered
       FROM zone z JOIN zone_cell zc USING (zone_seq)
      WHERE z.fld = 'programarea_key' AND z.{vc} = {DBI::dbQuoteString(con, pra_key)}
      ORDER BY zc.cell_id"))
  tibble::tibble(cell_id = as.integer(d$cell_id), pct_covered = as.numeric(d$pct_covered))
}

#' Precomputed component scores for a Program Area
#'
#' Reads the precomputed Program Area metrics from the `zone_metric`
#' table instead of aggregating across cells. Returns the same shape
#' as [scores_for_cells()] so it's a drop-in replacement for the
#' score / flower-plot pipeline when the area is a Program Area.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param pra_key Program Area key (e.g. "CGM")
#' @param metric_pattern regex to filter `metric.metric_key`
#'   (default: `"_ecoregion_rescaled$"`)
#' @details
#' A component with **no `zone_metric` row is absent, not zero**: v7.1 deletes the
#' row of a (zone, component) pair that fails the coverage floor, and a Program
#' Area whose component has no scored cell never had one. The inner joins here
#' preserve that — the row simply does not come back, and [mean_score()] averages
#' over what is present, which is how the published composite was computed.
#' **Reportability is the ABSENCE of the `_ecoregion_rescaled` row**, never the
#' `_prepctareaweighting` row, which stays behind even for a dropped pair.
#'
#' @return tibble(metric_key, score, component, even)
#' @importFrom DBI dbGetQuery dbQuoteString
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate filter
#' @importFrom glue glue
#' @importFrom stringr str_replace
#' @export
#' @concept calc
scores_for_pra <- function(con, pra_key,
                           metric_pattern = "_ecoregion_rescaled$") {
  vz <- sdm_val_col(con, "zone")
  vm <- sdm_val_col(con, "zone_metric")
  DBI::dbGetQuery(con, glue::glue(
    "SELECT m.metric_key, zm.{vm} AS score
       FROM zone z
       JOIN zone_metric zm USING (zone_seq)
       JOIN metric m       USING (metric_seq)
      WHERE z.fld = 'programarea_key'
        AND z.{vz} = {DBI::dbQuoteString(con, pra_key)}
        AND regexp_matches(m.metric_key, {DBI::dbQuoteString(con, metric_pattern)})
      ORDER BY m.metric_key")) |>
    tibble::as_tibble() |>
    dplyr::mutate(component = .component_of(.data$metric_key), even = 1) |>
    dplyr::filter(.data$component != "all")
}

# metric_key -> flower-petal component label. One implementation, because the three
# copies in the apps each spelled the regex differently.
.component_of <- function(metric_key)
  metric_key |>
    stringr::str_replace("extrisk_", "") |>
    stringr::str_replace("_ecoregion_rescaled", "") |>
    stringr::str_replace("_", " ")

#' Aggregate component scores across a set of cells
#'
#' The one scoring method (master-plan decisions D7 and D7b). Returns a
#' flower-plot-ready tibble: `metric_key`, `score`, `component`, `even`, plus
#' `coverage` and `mean_where_present`, which are what make a sliver readable as a
#' sliver instead of as a high score.
#'
#' @section The coverage blend (`blend = TRUE`, the default):
#' A published `zone_metric` is, exactly,
#' \deqn{\sum(\mathrm{coalesce}(val, 0) \cdot pct) / \sum(pct)}
#' over **every** cell of the zone — verified to reproduce all 795 v9 rows. The old
#' `scores_for_cells()` computed \eqn{\sum(val \cdot pct) / \sum(pct)} over only the
#' cells that HAVE the metric, which is the `_prepctareaweighting` intermediate, not
#' the published number. A drawn polygon exactly tracing a Program Area therefore
#' reported a different, systematically HIGHER score than the same area picked from
#' the list: up to 49.1 points on turtle (St George Basin, 1.4 % coverage), 20.3 on
#' primary producer, 9.6 on coral, and up to 6.28 on the composite.
#'
#' `blend = TRUE` computes the published method. `blend = FALSE` reproduces the old
#' reports and is kept only for that.
#'
#' @section The denominator (`denominator`):
#' `"study_area"` (default) first clips the supplied cells to the release's study
#' area with [cells_in_study_area()] — the cells present in `cell` with
#' `coalesce(in_usa, TRUE)`. "Absent means zero" is only sound where a value could
#' have existed, so land and foreign waters must not enter as zeros; a place half
#' over land would otherwise score half of what it is.
#'
#' `"all"` skips the clip and is exact zone parity (what `zone_cell` does, including
#' its non-`in_usa` rows). Measured on the 20 Program Areas the two differ by at most
#' 0.08 composite points, median 0.03.
#'
#' @section Reading the result:
#' `coverage` is the share of the denominator weight held by cells that carry the
#' component, and `mean_where_present` is the mean over just those cells. The
#' identity `score = coverage * mean_where_present` holds when `blend = TRUE`, so a
#' panel can say "17.7, over 1.4 % of the place" rather than implying 17.7 everywhere.
#' A component with no covered cell at all yields **no row** — never a zero — so it
#' is excluded from [mean_score()] exactly as an unreportable component is.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param cells a tibble from [cells_in_polygon()] or [cells_in_pra()], with columns
#'   `cell_id` and `pct_covered`
#' @param metric_pattern regex to filter `metric.metric_key`
#'   (default: `"_ecoregion_rescaled$"`; the `$` is what excludes the
#'   `_prepctareaweighting` rows)
#' @param blend use the published coverage blend (`TRUE`, default) or the old
#'   present-cells-only mean (`FALSE`)
#' @param denominator `"study_area"` (default, D7b) or `"all"` (exact zone parity)
#' @return tibble(metric_key, score, component, even, coverage, mean_where_present)
#' @importFrom DBI dbGetQuery dbQuoteString
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate filter
#' @importFrom glue glue
#' @export
#' @concept calc
scores_for_cells <- function(con, cells,
                             metric_pattern = "_ecoregion_rescaled$",
                             blend = TRUE,
                             denominator = c("study_area", "all")) {
  stopifnot(all(c("cell_id", "pct_covered") %in% names(cells)))
  denominator <- match.arg(denominator)
  if (identical(denominator, "study_area")) cells <- cells_in_study_area(con, cells)

  empty <- tibble::tibble(metric_key = character(), score = numeric(),
                          component = character(), even = numeric(),
                          coverage = numeric(), mean_where_present = numeric())
  if (!nrow(cells)) return(empty)

  vc   <- sdm_val_col(con, "cell_metric")
  vals <- paste(sprintf("(%d, %.10f)", as.integer(cells$cell_id),
                        as.numeric(cells$pct_covered)), collapse = ", ")
  # CROSS JOIN then LEFT JOIN, deliberately: an INNER JOIN on cell_metric is the
  # very bug being fixed — it makes a cell without the metric vanish from the
  # DENOMINATOR as well as the numerator.
  d <- DBI::dbGetQuery(con, glue::glue("
    WITH z AS (SELECT * FROM (VALUES {vals}) AS v(cell_id, pct_covered)),
    m AS (
      SELECT metric_seq, metric_key FROM metric
       WHERE regexp_matches(metric_key, {DBI::dbQuoteString(con, metric_pattern)})
    )
    SELECT m.metric_key,
           sum(COALESCE(cm.{vc}, 0) * z.pct_covered)                            AS num_blend,
           sum(CASE WHEN cm.{vc} IS NOT NULL THEN cm.{vc} * z.pct_covered END)  AS num_present,
           sum(CASE WHEN cm.{vc} IS NOT NULL THEN z.pct_covered ELSE 0 END)     AS w_present,
           sum(z.pct_covered)                                                   AS w_all
      FROM m CROSS JOIN z
      LEFT JOIN cell_metric cm
        ON cm.cell_id = z.cell_id AND cm.metric_seq = m.metric_seq
     GROUP BY m.metric_key
     ORDER BY m.metric_key"))
  if (!nrow(d)) return(empty)

  d <- d[d$w_present > 0, , drop = FALSE]     # no covered cell -> no row, not a zero
  if (!nrow(d)) return(empty)

  tibble::tibble(
    metric_key         = d$metric_key,
    score              = if (blend) d$num_blend / d$w_all else d$num_present / d$w_present,
    component          = .component_of(d$metric_key),
    even               = 1,
    coverage           = d$w_present / d$w_all,
    mean_where_present = d$num_present / d$w_present) |>
    dplyr::filter(.data$component != "all")
}

#' Resolve a release's column names for the cross-version queries
#'
#' The v8 rewrite renamed three things every taxon/model query depends on, and the
#' app-side copy of one such query was never migrated — which is why the v8
#' "Table of Species" tab came up empty with *"Can't select columns that don't
#' exist"*:
#'
#' | concept | v1-v7 | v8 |
#' | --- | --- | --- |
#' | taxon validity | `is_ok` | `is_valid_usa` |
#' | model id | `mdl_seq` | `ms_merge_key` (taxon) / `mdl_key` (`model_cell`) |
#' | cell value | `value` | `val` (`value` is reserved in DuckDB) |
#'
#' Resolved **per connection** by introspection, so one implementation serves every
#' release and no caller needs to know which generation it is talking to. Exported
#' because the versioned documentation asks the same question of the same published
#' tables — a second copy of the rule in the docs is exactly how a v3 page ends up
#' printing a v8 column name.
#'
#' Note the semantics the names hide: v7's `is_ok` already baked in the
#' marine/category cull, while v8's `is_valid_usa` only means "has >= 1 merged cell
#' in US waters", so scoring eligibility on v8 additionally needs `is_marine`
#' (returned as `marine`, `NA` when the release has no such column).
#'
#' @param con open connection to a release database, or to a set of views over one
#' @param mc_tbl name of the model-cell table to inspect, or `NULL` to skip it.
#'   Skipping matters on the server: inspecting `model_cell` there makes DuckDB LIST
#'   the S3 prefix just to read a schema, which fails outright.
#' @return a list with `valid`, `marine`, `tkey`, `mkey`, `val`
#' @examples
#' \dontrun{
#' con <- attach_atlas(version = "v7")
#' sdm_cols(con, mc_tbl = NULL)$valid   # "is_ok"
#' }
#' @importFrom DBI dbListFields
#' @export
#' @concept calc
sdm_cols <- function(con, mc_tbl = "model_cell") {
  taxon_cols <- DBI::dbListFields(con, "taxon")
  # `mc_tbl` matters on the server: inspecting `model_cell` there means DuckDB
  # LISTs the S3 prefix just to read its schema, which fails
  # ("SSL peer certificate ... HTTP GET .../serve/model_cell/") — so when the
  # local cell_model surface is being used, never touch model_cell at all.
  mc_cols    <- if (is.null(mc_tbl)) character(0) else DBI::dbListFields(con, mc_tbl)
  pick <- function(cands, have, what) {
    hit <- cands[cands %in% have]
    if (length(hit) == 0)
      stop("cannot resolve the ", what, " column; looked for: ",
           paste(cands, collapse = ", "), call. = FALSE)
    hit[1]
  }
  list(
    valid = pick(c("is_ok", "is_valid_usa"), taxon_cols, "taxon validity"),
    # v7's `is_ok` already baked in the marine/category cull; v8's
    # `is_valid_usa` only means "has >=1 merged cell in US waters", so the
    # scoring-eligibility rules must be applied explicitly (see below).
    marine = if ("is_marine" %in% taxon_cols) "is_marine" else NA_character_,
    tkey  = pick(c("mdl_seq", "ms_merge_key"), taxon_cols, "taxon model-id"),
    mkey  = if (length(mc_cols)) pick(c("mdl_seq", "mdl_key"), mc_cols, "model_cell model-id") else NA_character_,
    val   = if (length(mc_cols)) pick(c("value", "val"), mc_cols, "model_cell value") else NA_character_)
}

#' Name of the measurement column in a release's table
#'
#' Returns `"val"` or `"value"` — whichever the given table actually has.
#'
#' Releases disagree, and **so do the two forms of the same release**. v1-v7 named the
#' measurement `value` throughout (`zone`, `zone_metric`, `cell_metric`, `model_cell`).
#' v8 renamed it `val`, away from DuckDB's reserved word — but the release step *also*
#' writes a `value` alias into the served views, so a v8 `serve.duckdb` carries **both**
#' while the v8 source `sdm.duckdb` it was built from carries only `val`.
#'
#' The practical consequence, and the reason this is a function rather than a constant:
#' code that hardcodes `value` runs against every served release and fails against a v8
#' source database, while code that hardcodes `val` does the exact opposite. Both spellings
#' appeared in one app file. `val` is preferred where both exist, since that is the name
#' the data is actually stored under.
#'
#' The failure this prevents is not a clean missing-column error. In dplyr/dbplyr a bare
#' `value` with no such column resolves to a *function* further up the scope chain, so the
#' error reads `cannot coerce type 'closure' to vector of type 'character'` from somewhere
#' inside the SQL translator, far from the table that lacks the column.
#'
#' @param con open connection to a release database, or to a set of views over one
#' @param tbl name of the table to inspect, e.g. `"zone"`, `"cell_metric"`
#' @return `"val"` or `"value"`
#' @examples
#' \dontrun{
#' con <- attach_atlas(version = "v7")
#' sdm_val_col(con, "zone_metric")   # "value"
#' }
#' @importFrom DBI dbListFields
#' @export
#' @concept calc
sdm_val_col <- function(con, tbl) {
  stopifnot(length(tbl) == 1L)
  flds <- DBI::dbListFields(con, tbl)
  if ("val" %in% flds) return("val")
  if ("value" %in% flds) return("value")
  stop("`", tbl, "` has neither a `val` nor a `value` column; found: ",
       paste(flds, collapse = ", "), call. = FALSE)
}

# The one species-table aggregation, given SQL that yields (cell_id, pct_covered).
# Weighted by pct_covered so partially covered edge cells count proportionally.
.species_sql <- function(con, cells_sql, tiles = NULL) {
  # decide the source FIRST, then only inspect that table's schema
  use_cm <- "cell_model" %in% DBI::dbListTables(con)
  k <- sdm_cols(con, if (use_cm) NULL else "model_cell")
  # SCORING ELIGIBILITY, not just "has cells". v7 encoded this in `is_ok`; v8
  # splits it out, so without these the table lists non-marine and excluded
  # taxa — the v8 run surfaced a cane toad (amphibian) as the first row of the
  # study-area species table.
  marine_clause <- if (is.na(k$marine)) "" else glue::glue(" AND t.{k$marine}")

  # PREFER THE CELL-ORIENTED SURFACE. `model_cell` is partitioned by mdl_id, so
  # a per-cell question scans everything; `cell_model` holds the same rows
  # partitioned by a 2.5-degree spatial tile (see cell_model.R). It stores the
  # integer mdl_id rather than the mdl_key string, so join back through `model`.
  # `tiles` prunes to the relevant partitions — pass it whenever the cell ids are
  # known up front.
  if (use_cm) {
    k$val <- "val"
    tile_clause <- if (is.null(tiles)) "" else
      glue::glue(" WHERE tile IN ({paste(tiles, collapse = ', ')})")
    # cell_model stores whichever model id its generation uses, and they differ:
    # v8 stores the compact integer `mdl_id` (join `model` back to the STABLE
    # mdl_key that taxon carries), v7 stores `mdl_seq`, which taxon already joins
    # on directly. Assuming v8's shape made the v7 surface fail outright with
    # `Binder Error: Column "mdl_id" does not exist on left side of join`.
    cm_cols <- DBI::dbListFields(con, "cell_model")
    if ("mdl_id" %in% cm_cols) {
      k$mkey  <- "mdl_key"
      mc_from <- glue::glue(
        "(SELECT cm.cell_id, cm.val, mo.mdl_key FROM cell_model cm",
        " JOIN model mo USING (mdl_id)",
        "{tile_clause}) mc")
    } else {
      k$mkey  <- pick_cm <- if ("mdl_key" %in% cm_cols) "mdl_key" else
        if ("mdl_seq" %in% cm_cols) "mdl_seq" else
          stop("cell_model has no recognizable model id column", call. = FALSE)
      mc_from <- glue::glue(
        "(SELECT cm.cell_id, cm.val, cm.{pick_cm} FROM cell_model cm{tile_clause}) mc")
    }
  } else {
    mc_from <- "model_cell mc"
  }
  # The extinction-risk columns arrived in v3; v1 and v2 have no extrisk_code, er_score,
  # is_mmpa or is_mbta at all, so selecting them unconditionally made those two releases fail
  # outright with `Binder Error: ... does not have a column named "extrisk_code"`. Substitute
  # typed NULLs so the result KEEPS ITS SHAPE — .species_shares() multiplies by er_score, and a
  # missing column there would propagate as a silently absent share rather than an empty one.
  tx <- DBI::dbListFields(con, "taxon")
  col <- function(nm, expr, type) if (nm %in% tx) expr else sprintf("CAST(NULL AS %s)", type)
  er_code  <- col("extrisk_code", "t.extrisk_code",     "VARCHAR")
  er_score <- col("er_score",     "t.er_score / 100.0", "DOUBLE")
  is_mmpa  <- col("is_mmpa",      "t.is_mmpa",          "BOOLEAN")
  is_mbta  <- col("is_mbta",      "t.is_mbta",          "BOOLEAN")
  glue::glue("
    WITH z AS ({cells_sql})
    SELECT t.sp_cat,
           t.common_name              AS sp_common,
           t.scientific_name          AS sp_scientific,
           t.taxon_id,
           t.taxon_authority,
           {er_code}                  AS er_code,
           {er_score}                 AS er_score,
           {is_mmpa}                  AS is_mmpa,
           {is_mbta}                  AS is_mbta,
           CAST(mc.{k$mkey} AS VARCHAR) AS mdl_key,
           sum(c.area_km2 * z.pct_covered / 100.0)                         AS area_km2,
           sum(mc.{k$val} * z.pct_covered) / sum(z.pct_covered) / 100.0    AS avg_suit
    FROM {mc_from}
    JOIN z      USING (cell_id)
    JOIN cell c USING (cell_id)
    JOIN taxon t ON t.{k$tkey} = mc.{k$mkey}
    WHERE t.{k$valid}{marine_clause}
      AND t.sp_cat NOT IN ('reptile', 'amphibian')
    GROUP BY 1,2,3,4,5,6,7,8,9,10")
}

# A published `zone_taxon` carries the column names of ITS OWN generation, so reading one
# back is a schema question exactly like the live aggregation is. Three vintages exist:
#
#   v1, v2   rl_code, rl_score (already a fraction), no MMPA/MBTA flags
#   v3-v7    rl_code, er_score on the RAW 1-100 scale, mdl_seq, suit_rl*
#   v8       er_code, er_score as a fraction,          mdl_key, suit_er*
#
# The live path already normalises all of this (.species_sql aliases to mdl_key/er_code and
# divides er_score by 100); the precomputed path returned the stored columns verbatim, so
# species_for_zone() silently answered in a different shape depending on which release was
# open. That is what broke the v7 "Table of Species": the app selects the canonical names and
# got `Can't select columns that don't exist. x Column er_code doesn't exist`.
#
# The er_score SCALE is decided by the schema, never by inspecting the values: "the numbers
# look bigger than 1" is a guess that a release of all-least-concern taxa would get wrong.
# `rl_code` is the v1-v7 marker, and only v3-v7 pair it with a raw-scale er_score.
.zone_taxon_normalize <- function(d) {
  nm <- names(d)
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)

  mdl_key <- if ("mdl_key" %in% nm) chr(d$mdl_key) else
    if ("mdl_seq" %in% nm) chr(d$mdl_seq) else
      stop("zone_taxon has no recognizable model id column (mdl_key/mdl_seq)", call. = FALSE)

  er_score <- if ("er_score" %in% nm) {
    if ("rl_code" %in% nm) d$er_score / 100 else d$er_score   # v3-v7 stored 1-100
  } else if ("rl_score" %in% nm) d$rl_score else NA_real_     # v1/v2, already a fraction
  # a fraction is the contract .species_shares() and the app's formatPercentage() both assume,
  # so a future vintage that breaks the rule above must say so rather than render as 1000%
  stopifnot("zone_taxon er_score is not a 0-1 fraction" =
              all(is.na(er_score) | (er_score >= 0 & er_score <= 1)))

  out <- tibble::tibble(
    sp_cat          = d$sp_cat,
    sp_common       = d$sp_common,
    sp_scientific   = d$sp_scientific,
    taxon_id        = d$taxon_id,
    taxon_authority = d$taxon_authority,
    er_code         = if ("er_code" %in% nm) chr(d$er_code) else chr(d[["rl_code"]]),
    er_score        = as.numeric(er_score),
    is_mmpa         = if ("is_mmpa" %in% nm) as.logical(d$is_mmpa) else NA,
    is_mbta         = if ("is_mbta" %in% nm) as.logical(d$is_mbta) else NA,
    mdl_key         = mdl_key,
    area_km2        = as.numeric(d$area_km2),
    avg_suit        = as.numeric(d$avg_suit))
  # recomputed rather than read: the stored share columns are named per vintage too
  # (suit_rl/suit_rl_area/cat_suit_rl_area), and deriving them here is what guarantees they
  # agree with the er_score scale just resolved above
  .species_shares(out)
}

# shared post-aggregation: per-species and per-category contribution shares
.species_shares <- function(d) {
  d |>
    dplyr::mutate(
      suit_er      = .data$avg_suit * .data$er_score,
      suit_er_area = .data$avg_suit * .data$er_score * .data$area_km2) |>
    dplyr::group_by(.data$sp_cat) |>
    dplyr::mutate(cat_suit_er_area = sum(.data$suit_er_area, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(pct_cat = .data$suit_er_area / .data$cat_suit_er_area) |>
    dplyr::arrange(.data$sp_cat, .data$sp_scientific)
}

#' Species table aggregated across a set of cells
#'
#' Returns a tibble with one row per species, aggregated across the supplied
#' cell set with `pct_covered` weighting. Works against **both** the v7 and v8
#' schemas — the differing column names (`is_ok`/`is_valid_usa`,
#' `mdl_seq`/`ms_merge_key`, `value`/`val`) are resolved per connection.
#'
#' Use [species_for_zone()] instead for a whole zone (subregion / Program Area /
#' ecoregion): it resolves the zone's cells inside the database rather than
#' shipping hundreds of thousands of cell ids into the query.
#'
#' @param con a DBI connection to an `sdm.duckdb`
#' @param cells a tibble with `cell_id` and `pct_covered`, e.g. from
#'   [cells_in_polygon()]
#' @return tibble, one row per species; `mdl_key` is the model id as character
#'   (the v7 `mdl_seq` or the v8 `mdl_key`)
#' @export
#' @concept calc
species_for_cells <- function(con, cells) {
  stopifnot(all(c("cell_id", "pct_covered") %in% names(cells)))
  vals <- paste(
    sprintf("(%d, %s)", as.integer(cells$cell_id), as.numeric(cells$pct_covered)),
    collapse = ", ")
  cells_sql <- glue::glue("SELECT * FROM (VALUES {vals}) AS v(cell_id, pct_covered)")
  # resolve the tile width from THIS database — v7 and v8 tile on different grids,
  # and a mismatch prunes away the very rows being sought, silently (a wrong tile
  # id is still a valid tile id, so the query just returns fewer/no species)
  tiles <- cell_model_tiles(cells$cell_id, ncol = cell_grid_ncol(con))
  DBI::dbGetQuery(con, .species_sql(con, cells_sql, tiles = tiles)) |>
    dplyr::as_tibble() |>
    .species_shares()
}

#' Build the precomputed zone x taxon summary table
#'
#' Computes [species_for_zone()] for **every** zone in the `zone` table and
#' writes the result as a single `zone_taxon` table.
#'
#' WHY PRECOMPUTE. v7 shipped a `zone_taxon` table and v8 dropped it, on the
#' assumption the app could aggregate live. It can locally — but **not on the
#' server**, which holds only the KB-sized `serve.duckdb` whose `model_cell` is a
#' view over S3 Parquet *partitioned by `mdl_id`* for per-model point reads
#' (titiler tiles). A zone-wide aggregation there means listing and scanning the
#' whole 580M-row dataset over HTTPS; in practice it fails outright with
#' `IO Error: ... HTTP GET .../serve/model_cell/`. Precomputing here — where
#' `model_cell` is local — turns that into a few-MB table the app just reads.
#'
#' @param con a DBI connection to the FULL `sdm.duckdb` (local `model_cell`)
#' @param overwrite replace an existing `zone_taxon` table
#' @return invisibly, the number of rows written
#' @export
#' @concept calc
build_zone_taxon <- function(con, overwrite = TRUE) {
  zones <- DBI::dbGetQuery(con, "SELECT DISTINCT fld, val FROM zone ORDER BY fld, val")
  stopifnot("no zones found" = nrow(zones) > 0)
  out <- vector("list", nrow(zones))
  for (i in seq_len(nrow(zones))) {
    # use_precomputed = FALSE: this function BUILDS zone_taxon, so reading an existing
    # one here would "rebuild" it out of its own previous output — every re-run after a
    # scoring change would copy the stale rows forward and report success.
    d <- species_for_zone(con, zones$fld[i], zones$val[i], use_precomputed = FALSE)
    if (nrow(d) == 0) next
    out[[i]] <- dplyr::mutate(d, zone_fld = zones$fld[i], zone_value = zones$val[i],
                              .before = 1)
  }
  d_all <- dplyr::bind_rows(out)
  if (overwrite && "zone_taxon" %in% DBI::dbListTables(con))
    DBI::dbExecute(con, "DROP TABLE zone_taxon")
  DBI::dbWriteTable(con, "zone_taxon", as.data.frame(d_all))
  invisible(nrow(d_all))
}

#' Species table aggregated across a zone
#'
#' One row per species within a named zone (`subregion_key`, `programarea_key`
#' or `ecoregion_key`), aggregated with `pct_covered` weighting.
#'
#' Computed live from `zone_cell` + `model_cell` + `taxon` rather than read from
#' a precomputed table: v7 shipped a `zone_taxon` table, but **v8 does not build
#' one**, which left the app's species table broken. Measured on the v8 database,
#' the largest zone (`subregion_key = "USA"`, ~349k cells, ~10k species) takes
#' ~5 s, so precomputation is not required.
#'
#' @param con a DBI connection to an `sdm.duckdb`
#' @param zone_fld zone field, e.g. `"programarea_key"`
#' @param zone_val zone value, e.g. `"GAA"`
#' @param use_precomputed read an existing `zone_taxon` table when present.
#'   `FALSE` forces the live aggregation — which is what [build_zone_taxon()]
#'   needs, since it must not rebuild the table out of its own previous output.
#' @return tibble, same shape as [species_for_cells()], whichever release's
#'   schema the connection holds
#' @export
#' @concept calc
species_for_zone <- function(con, zone_fld, zone_val, use_precomputed = TRUE) {
  stopifnot(length(zone_fld) == 1L, length(zone_val) == 1L)

  # PREFER THE PRECOMPUTED TABLE. On the server `con` is the KB-sized
  # serve.duckdb whose `model_cell` is an S3 view partitioned by mdl_id for
  # per-model point reads, so aggregating live there means listing + scanning
  # ~580M rows over HTTPS and fails with an S3 IO error. `zone_taxon` (built by
  # build_zone_taxon() where model_cell is local, and released alongside the
  # other tables) makes this a small indexed read. Falling back to the live
  # aggregation keeps local development working before/without that table.
  if (use_precomputed && "zone_taxon" %in% DBI::dbListTables(con)) {
    q <- glue::glue(
      "SELECT * FROM zone_taxon ",
      "WHERE zone_fld = {DBI::dbQuoteString(con, zone_fld)} ",
      "AND zone_value = {DBI::dbQuoteString(con, zone_val)}")
    d <- dplyr::as_tibble(DBI::dbGetQuery(con, q))
    # normalised, NOT returned verbatim: a v1-v7 table speaks its own generation's
    # column names, and the caller asked for a species table, not a v7 species table
    return(.zone_taxon_normalize(d))
  }

  cells_sql <- glue::glue(
    "SELECT zc.cell_id, zc.pct_covered FROM zone_cell zc ",
    "JOIN zone zn USING (zone_seq) ",
    "WHERE zn.fld = {DBI::dbQuoteString(con, zone_fld)} ",
    "AND zn.val = {DBI::dbQuoteString(con, zone_val)}")
  DBI::dbGetQuery(con, .species_sql(con, cells_sql)) |>
    dplyr::as_tibble() |>
    .species_shares()
}

#' Weighted mean of component scores
#'
#' Convenience wrapper returning the weighted mean of the `score`
#' column from [scores_for_cells()], weighted by `even`.
#'
#' @param d_scores tibble from [scores_for_cells()]
#' @return a numeric scalar
#' @export
#' @concept calc
mean_score <- function(d_scores) {
  stats::weighted.mean(d_scores$score, d_scores$even, na.rm = TRUE)
}

# silence R CMD check NOTEs for dbplyr/dplyr non-standard evaluation
utils::globalVariables(c(
  "metric_key", "metric_seq", "cell_id", "pct_covered", "value",
  "component", "even", "is_ok", "common_name", "scientific_name",
  "taxon_id", "taxon_authority", "extrisk_code", "er_score",
  "is_mmpa", "is_mbta", "mdl_seq", "area_km2", "sp_cat",
  "sp_common", "sp_scientific", "er_code", "avg_suit",
  "suit_er_area", "cat_suit_er_area",
  "fld", "zone_seq"))
