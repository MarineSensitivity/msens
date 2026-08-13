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
#' queried and cannot disagree with it. Two paths, chosen automatically:
#'
#' * **v8** (`cell` carries `lon`/`lat`) — a SQL bbox select on `cell` picks the
#'   candidates and `sf` computes exact coverage on just those. No raster is
#'   touched. For a 2x1.5-degree area this is **~0.02 s** against ~35 s to read
#'   the whole cell-id raster, and it joins `cell_model`, which is already
#'   cell-oriented.
#' * **v7** (no `lon`/`lat`) — falls back to [cell_id_raster()], the 0-360
#'   regional raster that IS v7's grid.
#'
#' Passing a [`terra::SpatRaster`] directly still works, but nothing can then
#' verify it matches the database — see [cell_id_raster()] for how that failed
#' silently on v8.
#'
#' @param poly an sf polygon (assumed or transformable to EPSG:4326)
#' @param src a DBI connection (**preferred**), or a single-layer
#'   [`terra::SpatRaster`] of integer cell ids
#' @param res grid resolution in degrees, SQL path only (default `0.05`)
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
  if (methods::is(src, "DBIConnection")) {
    if (.cell_has_lonlat(src))
      return(.cells_in_polygon_db(poly, src, res))
    src <- cell_id_raster()          # v7: `cell` has no lon/lat
  }
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

# split a longitude range into 1-2 ranges inside [-180,180] (antimeridian-safe)
.lon_ranges <- function(x0, x1) {
  if (x1 - x0 >= 360) return(list(c(-180, 180)))
  wrap <- function(x) ((x + 180) %% 360) - 180
  a <- wrap(x0); b <- wrap(x1)
  if (a <= b) list(c(a, b)) else list(c(a, 180), c(-180, b))
}

# v8 path: bbox-select candidates in SQL, then exact coverage on just those.
# Coverage is computed PLANAR in degrees, which is what terra's `cover = TRUE`
# does in the raster's CRS — so the two paths report pct_covered the same way.
.cells_in_polygon_db <- function(poly, con, res = 0.05) {
  empty <- tibble::tibble(cell_id = integer(), pct_covered = numeric())
  g <- sf::st_union(sf::st_geometry(sf::st_transform(poly, 4326)))
  g <- tryCatch(sf::st_wrap_dateline(g), error = function(e) g)
  bb <- sf::st_bbox(g)
  h  <- res / 2
  lon_sql <- paste(vapply(
    .lon_ranges(bb[["xmin"]] - h, bb[["xmax"]] + h),
    function(r) sprintf("(lon BETWEEN %.10f AND %.10f)", r[1], r[2]), ""),
    collapse = " OR ")
  cand <- DBI::dbGetQuery(con, sprintf(
    "SELECT cell_id, lon, lat FROM cell WHERE (%s) AND lat BETWEEN %.10f AND %.10f",
    lon_sql, bb[["ymin"]] - h, bb[["ymax"]] + h))
  if (!nrow(cand)) return(empty)
  gp <- sf::st_set_crs(g, NA_character_)
  bx <- sf::st_sfc(lapply(seq_len(nrow(cand)), function(i) sf::st_polygon(list(cbind(
    cand$lon[i] + c(-h, h, h, -h, -h),
    cand$lat[i] + c(-h, -h, h, h, -h))))))
  hit <- sf::st_intersects(bx, gp, sparse = FALSE)[, 1]
  if (!any(hit)) return(empty)
  inter <- suppressWarnings(sf::st_intersection(bx[hit], gp))
  pct   <- round(as.numeric(sf::st_area(inter)) / (res * res) * 100)
  keep  <- pct > 0
  tibble::tibble(
    cell_id     = as.integer(cand$cell_id[hit][keep]),
    pct_covered = pct[keep])
}

#' Cells belonging to a Program Area zone
#'
#' Fast lookup of the cells making up a Program Area by reading
#' directly from the `zone` / `zone_cell` tables, avoiding the
#' `terra::rasterize()` cost paid by [cells_in_polygon()]. Returns
#' the same shape (`cell_id`, `pct_covered`) so downstream helpers
#' can consume it interchangeably; `pct_covered` is always 100
#' because `zone_cell` membership is binary.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param pra_key Program Area key (e.g. "CGM")
#' @return tibble(cell_id integer, pct_covered integer = 100L)
#' @importFrom dplyr tbl filter select inner_join collect mutate join_by
#' @export
#' @concept calc
cells_in_pra <- function(con, pra_key) {
  dplyr::tbl(con, "zone") |>
    dplyr::filter(fld == "programarea_key", value == !!pra_key) |>
    dplyr::select(zone_seq) |>
    dplyr::inner_join(
      dplyr::tbl(con, "zone_cell") |> dplyr::select(zone_seq, cell_id),
      by = dplyr::join_by(zone_seq)) |>
    dplyr::select(cell_id) |>
    dplyr::collect() |>
    dplyr::mutate(pct_covered = 100L)
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
#' @return tibble(metric_key, score, component, even)
#' @importFrom dplyr tbl filter select inner_join collect mutate join_by
#' @importFrom stringr str_detect str_replace
#' @export
#' @concept calc
scores_for_pra <- function(con, pra_key,
                           metric_pattern = "_ecoregion_rescaled$") {
  dplyr::tbl(con, "zone") |>
    dplyr::filter(fld == "programarea_key", value == !!pra_key) |>
    dplyr::select(zone_seq) |>
    dplyr::inner_join(
      dplyr::tbl(con, "zone_metric") |>
        dplyr::select(zone_seq, metric_seq, score = value),
      by = dplyr::join_by(zone_seq)) |>
    dplyr::inner_join(
      dplyr::tbl(con, "metric") |>
        dplyr::filter(stringr::str_detect(metric_key, metric_pattern)),
      by = dplyr::join_by(metric_seq)) |>
    dplyr::select(metric_key, score) |>
    dplyr::collect() |>
    dplyr::mutate(
      component = metric_key |>
        stringr::str_replace("extrisk_", "") |>
        stringr::str_replace("_ecoregion_rescaled", "") |>
        stringr::str_replace("_", " "),
      even = 1) |>
    dplyr::filter(component != "all")
}

#' Aggregate component scores across a set of cells
#'
#' Weighted-mean aggregation of `cell_metric` across a set of cells;
#' returns a flower-plot-ready tibble with columns
#' `metric_key`, `score`, `component`, `even`.
#'
#' @param con a DBI connection (e.g. from [sdm_db_con()])
#' @param cells a tibble from [cells_in_polygon()], with columns
#'   `cell_id` and `pct_covered`
#' @param metric_pattern regex to filter `metric.metric_key`
#'   (default: `"_ecoregion_rescaled$"`)
#' @return tibble(metric_key, score, component, even)
#' @importFrom dplyr tbl filter inner_join group_by summarize collect mutate
#' @importFrom dbplyr copy_inline
#' @importFrom stringr str_detect str_replace
#' @export
#' @concept calc
scores_for_cells <- function(con, cells,
                             metric_pattern = "_ecoregion_rescaled$") {
  cells_t <- dbplyr::copy_inline(con, cells)
  dplyr::tbl(con, "metric") |>
    dplyr::filter(stringr::str_detect(metric_key, metric_pattern)) |>
    dplyr::inner_join(dplyr::tbl(con, "cell_metric"), by = "metric_seq") |>
    dplyr::inner_join(cells_t, by = "cell_id") |>
    dplyr::group_by(metric_key) |>
    dplyr::summarize(
      score = sum(value * pct_covered, na.rm = TRUE) /
              sum(pct_covered, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(
      component = metric_key |>
        stringr::str_replace("extrisk_", "") |>
        stringr::str_replace("_ecoregion_rescaled", "") |>
        stringr::str_replace("_", " "),
      even = 1) |>
    dplyr::filter(component != "all")
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
    d <- species_for_zone(con, zones$fld[i], zones$val[i])
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
#' @return tibble, same shape as [species_for_cells()]
#' @export
#' @concept calc
species_for_zone <- function(con, zone_fld, zone_val) {
  stopifnot(length(zone_fld) == 1L, length(zone_val) == 1L)

  # PREFER THE PRECOMPUTED TABLE. On the server `con` is the KB-sized
  # serve.duckdb whose `model_cell` is an S3 view partitioned by mdl_id for
  # per-model point reads, so aggregating live there means listing + scanning
  # ~580M rows over HTTPS and fails with an S3 IO error. `zone_taxon` (built by
  # build_zone_taxon() where model_cell is local, and released alongside the
  # other tables) makes this a small indexed read. Falling back to the live
  # aggregation keeps local development working before/without that table.
  if ("zone_taxon" %in% DBI::dbListTables(con)) {
    q <- glue::glue(
      "SELECT * FROM zone_taxon ",
      "WHERE zone_fld = {DBI::dbQuoteString(con, zone_fld)} ",
      "AND zone_value = {DBI::dbQuoteString(con, zone_val)}")
    d <- dplyr::as_tibble(DBI::dbGetQuery(con, q))
    return(dplyr::arrange(d, .data$sp_cat, .data$sp_scientific))
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
