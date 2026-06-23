# generate a static STAC catalog (with the sdm: extension) from the msens stores ----
# see the sdm extension: https://marinesensitivity.github.io/stac-sdm
# the catalog describes the cloud-native artifacts msens already writes (GeoParquet,
# COGs) plus the live DuckDB-SQL TiTiler endpoint, with the prediction interval
# (month/season) made explicit via the datacube extension + sdm:* phenology fields.

SDM_EXT_URL    <- "https://marinesensitivity.github.io/stac-sdm/v1.0.0/schema.json"
DATACUBE_EXT   <- "https://stac-extensions.github.io/datacube/v2.2.0/schema.json"
TABLE_EXT      <- "https://stac-extensions.github.io/table/v1.2.0/schema.json"
RASTER_EXT     <- "https://stac-extensions.github.io/raster/v1.1.0/schema.json"
WML_EXT        <- "https://stac-extensions.github.io/web-map-links/v1.2.0/schema.json"
ALT_EXT        <- "https://stac-extensions.github.io/alternate-assets/v1.2.0/schema.json"
VERSION_EXT    <- "https://stac-extensions.github.io/version/v1.2.0/schema.json"
SCI_EXT        <- "https://stac-extensions.github.io/scientific/v1.0.0/schema.json"
ITEM_ASSETS_EXT<- "https://stac-extensions.github.io/item-assets/v1.0.0/schema.json"

#' Default configuration (base URLs) for STAC generation
#'
#' Mirrors the `is_prod`/`pmtiles_base_url` pattern: defaults point at the public
#' marinesensitivity.org hosts. Override for local or BOEM-internal deployments.
#'
#' @param version data version (e.g. "v7")
#' @return named list of base URLs + the study-area bbox
#' @export
#' @concept stac
stac_cfg <- function(version = "v7") {
  list(
    version      = version,
    # static catalog tree (served from /share/public/stac)
    stac_base    = "https://file.marinesensitivity.org/stac",
    # versioned derived data: GeoParquet, COGs, GeoPackages (served from /share/data/derived)
    data_base    = "https://file.marinesensitivity.org/derived",
    file_base    = "https://file.marinesensitivity.org",
    # custom DuckDB-SQL TiTiler factory (mounted at /msens on the titiler host)
    titiler_base = "https://titiler.marinesensitivity.org/msens",
    # pg_tileserv vector tiles
    pg_base      = "https://tile.marinesensitivity.org",
    # full study-area bbox [W,S,E,N] (US EEZ incl. Pacific territories + Alaska)
    bbox         = c(-180, -18, 180, 75))
}

#' Encode SQL as urlsafe base64 for the TiTiler `sql=` query parameter
#'
#' Matches `server/titiler/factory.py::_decode_sql` (`urlsafe_b64decode`, padding
#' optional). Use the result directly in `?sql=` on the /msens endpoints.
#'
#' @param sql a single `SELECT cell_id, value ...` statement
#' @return urlsafe base64 string (padding stripped)
#' @importFrom base64enc base64encode
#' @export
#' @concept stac
sdm_sql_b64 <- function(sql) {
  b64 <- base64enc::base64encode(charToRaw(sql))
  sub("=+$", "", chartr("+/", "-_", b64))
}

# ---- internal mappers -------------------------------------------------------

# dataset.response_type -> sdm:response_type enum
.sdm_response_type <- function(x) {
  switch(
    x %or% "mixed",
    suitability = "suitability",
    binary      = "range",
    probability = "probability",
    density     = "density",
    biomass     = "biomass",
    occurrence  = "occurrence",
    "mixed")
}

# ds_key -> sdm:method (open vocab); fall back by source
.sdm_method <- function(ds_key, source_broad = NA_character_) {
  m <- c(
    am_0.05             = "env_envelope",
    bl                  = "expert_range_polygon",
    rng_iucn            = "expert_range_polygon",
    rng_fws             = "expert_range_polygon",
    rng_turtle_swot_dps = "expert_range_polygon",
    ch_fws              = "critical_habitat",
    ch_nmfs             = "critical_habitat",
    ca_nmfs             = "core_area",
    ms_merge            = "derived_merge",
    gm                  = "density_surface_model",
    nc_atl_birds_dens   = "density_surface_model",
    nc_pac_birds_dens   = "density_surface_model")
  if (ds_key %in% names(m)) return(unname(m[ds_key]))
  "expert_range_polygon"
}

# value unit + nominal range by response type
.sdm_value <- function(response_type) {
  switch(
    response_type,
    suitability = list(unit = "percent",  range = c(0, 100)),
    density     = list(unit = "n_per_km2", range = NULL),
    biomass     = list(unit = "kg_per_km2", range = NULL),
    range       = list(unit = "unitless", range = c(0, 1)),
    probability = list(unit = "unitless", range = c(0, 1)),
    list(unit = NULL, range = NULL))
}

`%or%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

.iso <- function(d) {
  if (is.null(d) || is.na(d)) return(NULL)
  paste0(format(as.Date(d), "%Y-%m-%d"), "T00:00:00Z")
}

# write one node to disk as pretty JSON (arrays preserved at length 1 via I())
.stac_write <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    obj, path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  invisible(path)
}

# ---- node builders ----------------------------------------------------------

#' Build the SDM properties block shared by Collections and Items
#'
#' @param ds one-row data.frame from the `dataset` table
#' @return named list of core + optional sdm: fields
#' @export
#' @concept stac
stac_sdm_props <- function(ds) {
  rt <- .sdm_response_type(ds$response_type)
  v  <- .sdm_value(rt)
  ti <- ds$temporal_res %or% "static"
  p  <- list(
    `sdm:response_type`     = rt,
    `sdm:method`            = .sdm_method(ds$ds_key, ds$source_broad),
    `sdm:source_authority`  = ds$source_broad %or% "MarineSensitivity",
    `sdm:temporal_interval` = ti)
  if (!is.null(v$unit))            p[["sdm:value_unit"]]  <- v$unit
  if (!is.null(v$range))           p[["sdm:value_range"]] <- I(v$range)
  if (ti %in% c("monthly", "seasonal")) p[["sdm:climatological"]] <- TRUE
  if (identical(ds$ds_key, "ms_merge")) p[["sdm:method_detail"]] <-
    "max / multiplicative merge across contributing datasets"
  env_beg <- .iso(ds$date_env_beg); env_end <- .iso(ds$date_env_end)
  if (!is.null(env_beg) || !is.null(env_end))
    p[["sdm:env_datetime"]] <- I(c(env_beg %or% NA, env_end %or% NA))
  p
}

#' Dataset Collection node
#'
#' @param ds one-row data.frame from the `dataset` table
#' @param cfg config from [stac_cfg()]
#' @return STAC Collection as a list
#' @export
#' @concept stac
stac_dataset_collection <- function(ds, cfg, item_files = character()) {
  obs_beg <- .iso(ds$date_obs_beg); obs_end <- .iso(ds$date_obs_end)
  if (is.null(obs_beg)) obs_beg <- paste0(ds$year_pub %or% 2025, "-01-01T00:00:00Z")
  has_cite <- !is.null(ds$citation) && !is.na(ds$citation)
  exts <- if (has_cite) c(VERSION_EXT, SCI_EXT, ITEM_ASSETS_EXT, SDM_EXT_URL)
          else          c(VERSION_EXT, ITEM_ASSETS_EXT, SDM_EXT_URL)
  node <- c(list(
    type           = "Collection",
    stac_version   = "1.0.0",
    stac_extensions= I(exts),
    id             = paste0("msens-", cfg$version, "-", ds$ds_key),
    title          = ds$name_display %or% ds$name_short %or% ds$ds_key,
    description    = ds$description %or% ds$name_short %or% ds$ds_key,
    license        = "CC-BY-4.0",
    extent         = list(
      spatial  = list(bbox     = list(I(cfg$bbox))),
      temporal = list(interval = list(I(c(obs_beg, obs_end %or% NA))))),
    version        = cfg$version,
    deprecated     = FALSE),
    stac_sdm_props(ds),
    list(
      item_assets = list(data = list(
        type  = "application/vnd.apache.parquet",
        roles = I("data"),
        title = "Per-taxon surfaces (GeoParquet of model_cell rows)")),
      links = c(
        list(
          list(rel = "root",   href = "../../catalog.json"),
          list(rel = "parent", href = "../collection.json"),
          list(rel = "self",   href = paste0(
            cfg$stac_base, "/", cfg$version, "/", ds$ds_key, "/collection.json"))),
        lapply(item_files, function(f) list(rel = "item", href = paste0("./", f))))))
  if (has_cite)
    node[["sci:citation"]] <- ds$citation
  node
}

#' Model-surface Item for a (DuckDB-backed) dataset
#'
#' Describes the dataset's per-taxon surfaces as a static GeoParquet asset plus
#' the live DuckDB-SQL TiTiler endpoint (as a web-map link + an alternate asset),
#' parameterized by `{mdl_seq}`. If the dataset has more than one prediction
#' interval, a datacube temporal/season dimension enumerates them.
#'
#' @param ds one-row data.frame from `dataset`
#' @param cfg config from [stac_cfg()]
#' @param time_periods character vector of distinct `model.time_period` for the dataset
#' @return STAC Item as a list
#' @export
#' @concept stac
stac_model_cell_item <- function(ds, cfg, time_periods = NULL) {
  id     <- paste0("msens-", cfg$version, "-", ds$ds_key, "-model_cell")
  props  <- stac_sdm_props(ds)
  obs_beg<- .iso(ds$date_obs_beg); obs_end <- .iso(ds$date_obs_end)
  if (!is.null(obs_beg) && !is.null(obs_end)) {
    props$datetime <- NULL; props$start_datetime <- obs_beg; props$end_datetime <- obs_end
  } else {
    props$datetime <- paste0(ds$year_pub %or% 2025, "-01-01T00:00:00Z")
  }
  props$title       <- paste0(ds$name_display %or% ds$ds_key, " surfaces, ", cfg$version)
  props$description  <- paste0(
    "Per-taxon model surfaces stored as model_cell rows; served statically as ",
    "GeoParquet and dynamically via the DuckDB-SQL TiTiler endpoint.")

  exts <- c(TABLE_EXT, WML_EXT, ALT_EXT, SDM_EXT_URL)

  # multi-interval -> datacube dimension (months or seasons)
  is_monthly  <- identical(ds$temporal_res, "monthly")
  is_seasonal <- identical(ds$temporal_res, "seasonal")
  if (!is.null(time_periods) && length(time_periods) > 1 && (is_monthly || is_seasonal)) {
    exts <- c(DATACUBE_EXT, exts)
    if (is_monthly) {
      mons <- sort(unique(sub("/.*$", "", time_periods)))
      props[["cube:dimensions"]] <- list(month = list(
        type = "temporal", extent = I(c(paste0(mons[1], "-01T00:00:00Z"), NA)),
        values = I(mons), step = "P1M"))
    } else {
      props[["cube:dimensions"]] <- list(season = list(
        type = "season",
        values = I(c("winter", "spring", "summer", "fall")),
        description = "Climatological season of the prediction"))
    }
  }

  # SQL that retrieves a per-taxon surface (literal {mdl_seq} placeholder, filled by clients)
  sql_tmpl <- "SELECT cell_id, value FROM model_cell WHERE mdl_seq = {mdl_seq}"
  # a representative baked URL (first mdl_seq) for the web-map link
  sql_ex   <- "SELECT cell_id, value FROM model_cell WHERE mdl_seq = 1"
  b64      <- sdm_sql_b64(sql_ex)
  rescale  <- if (.sdm_response_type(ds$response_type) == "suitability") "0,100" else NULL
  tj       <- glue::glue("{cfg$titiler_base}/tilejson.json?sql={b64}&colormap=spectral_r")
  xyz      <- glue::glue("{cfg$titiler_base}/tiles/{{z}}/{{x}}/{{y}}.png?sql={b64}&colormap=spectral_r")
  if (!is.null(rescale)) { tj <- paste0(tj, "&rescale=", rescale); xyz <- paste0(xyz, "&rescale=", rescale) }

  pq_href <- glue::glue("{cfg$data_base}/{cfg$version}/sdm_parquet/model_cell.parquet/")

  list(
    type            = "Feature",
    stac_version    = "1.0.0",
    stac_extensions = I(exts),
    id              = id,
    collection      = paste0("msens-", cfg$version, "-", ds$ds_key),
    bbox            = I(cfg$bbox),
    geometry        = list(type = "Polygon", coordinates = .bbox_poly(cfg$bbox)),
    properties      = props,
    assets          = list(
      data = list(
        href  = pq_href,
        type  = "application/vnd.apache.parquet",
        roles = I("data"),
        title = "model_cell surfaces (partitioned GeoParquet; filter by mdl_seq)",
        `table:primary_geometry` = "cell_id",
        `table:columns` = list(
          list(name = "mdl_seq", type = "int32",  description = "model id (FK model.mdl_seq)"),
          list(name = "cell_id", type = "int32",  description = "grid cell id (FK cell.cell_id)"),
          list(name = "value",   type = "double", description = "model value")),
        alternate = list(duckdb_sql = list(
          href = cfg$titiler_base,
          `alternate:name` = "Live DuckDB-SQL surface",
          roles = I("data"),
          `sdm:sql_template` = sql_tmpl)))),
    links = list(
      list(rel = "root",       href = "../../catalog.json"),
      list(rel = "parent",     href = "../collection.json"),
      list(rel = "collection", href = "../collection.json"),
      # web-map-links extends Links (not Assets): the live TiTiler endpoints
      list(rel = "xyz",        href = xyz, type = "image/png",
           title = "Rendered raster tiles via TiTiler (DuckDB SQL); re-bake sql= per mdl_seq"),
      list(rel = "tilejson",   href = tj,  type = "application/json",
           title = "TileJSON for the DuckDB-SQL surface"),
      list(rel = "self",       href = paste0(
        cfg$stac_base, "/", cfg$version, "/", ds$ds_key, "/", id, ".json"))))
}

#' Seasonal per-species Item from the NCCOS season-COG metadata
#'
#' One Item per (ds_key, sp_code); one COG Asset per season (the public
#' season-named GeoTIFFs), each tagged with `sdm:season` and `raster:bands`.
#'
#' @param d_sp rows of `nc_models.csv` for one (ds_key, sp_code)
#' @param cfg config from [stac_cfg()]
#' @param taxon optional list(scientific_name=, common_name=, group=, authorities=)
#' @return STAC Item as a list
#' @export
#' @concept stac
stac_season_cog_item <- function(d_sp, cfg, taxon = NULL) {
  ds_key  <- d_sp$ds_key[1]; sp_code <- d_sp$sp_code[1]
  seasons <- unique(d_sp$season)
  id      <- paste0("msens-", cfg$version, "-", ds_key, "-", sp_code)
  props   <- list(
    datetime               = NULL,
    start_datetime         = "1980-01-01T00:00:00Z",
    end_datetime           = "2017-12-31T23:59:59Z",
    title                  = paste0("NCCOS seasonal density: ", sp_code, ", ", cfg$version),
    description            = "Long-term seasonal relative density; one COG asset per season.",
    `sdm:response_type`    = "density",
    `sdm:value_unit`       = "n_per_km2",
    `sdm:method`           = "density_surface_model",
    `sdm:source_authority` = "NOAA NCCOS",
    `sdm:temporal_interval`= "seasonal",
    `sdm:climatological`   = TRUE,
    `cube:dimensions`      = list(season = list(
      type = "season", values = I(seasons),
      description = "Climatological season of the prediction")))
  if (!is.null(taxon)) props[["sdm:taxon"]] <- taxon

  # one asset per season, using the n_per_km2 band row for the href; bands from all vars
  assets <- list()
  for (s in seasons) {
    d_s  <- d_sp[d_sp$season == s, ]
    href <- d_s$cog_url[1]
    bands <- lapply(seq_len(nrow(d_s)), function(i) list(
      `data_type` = "float32", unit = "n_per_km2",
      description = d_s$var[i], `spatial_resolution` = 5500))
    assets[[s]] <- list(
      href = href,
      type = "image/tiff; application=geotiff; profile=cloud-optimized",
      roles = I("data"),
      title = paste0(s, " relative density"),
      `sdm:season` = s,
      `raster:bands` = bands)
  }

  list(
    type            = "Feature",
    stac_version    = "1.0.0",
    stac_extensions = I(c(RASTER_EXT, DATACUBE_EXT, SDM_EXT_URL)),
    id              = id,
    collection      = paste0("msens-", cfg$version, "-", ds_key),
    bbox            = I(c(-82, 24, -65, 45)),
    geometry        = list(type = "Polygon", coordinates = .bbox_poly(c(-82, 24, -65, 45))),
    properties      = props,
    assets          = assets,
    links = list(
      list(rel = "root",       href = "../../catalog.json"),
      list(rel = "parent",     href = "../collection.json"),
      list(rel = "collection", href = "../collection.json"),
      list(rel = "self",       href = paste0(
        cfg$stac_base, "/", cfg$version, "/", ds_key, "/", id, ".json"))))
}

# closed [W,S,E,N] -> GeoJSON polygon ring (nested list for jsonlite)
.bbox_poly <- function(b) {
  ring <- list(
    c(b[1], b[2]), c(b[3], b[2]), c(b[3], b[4]), c(b[1], b[4]), c(b[1], b[2]))
  list(lapply(ring, function(xy) I(xy)))
}

#' Version Collection node (parent of the dataset Collections)
#'
#' @param ds_keys character vector of dataset keys
#' @param cfg config from [stac_cfg()]
#' @return STAC Collection as a list
#' @export
#' @concept stac
stac_version_collection <- function(ds_keys, cfg) {
  child <- lapply(ds_keys, function(k) list(
    rel = "child", href = paste0("./", k, "/collection.json")))
  c(list(
    type            = "Collection",
    stac_version    = "1.0.0",
    stac_extensions = I(VERSION_EXT),
    id              = paste0("msens-", cfg$version),
    title           = paste0("MarineSensitivity SDMs, ", cfg$version),
    description     = paste0(
      "Species distribution / sensitivity model products for MarineSensitivity ",
      cfg$version, ", one child Collection per source dataset."),
    license         = "CC-BY-4.0",
    version         = cfg$version,
    extent          = list(
      spatial  = list(bbox     = list(I(cfg$bbox))),
      temporal = list(interval = list(I(c("2019-01-01T00:00:00Z", NA))))),
    links = c(
      list(
        list(rel = "root",   href = "../catalog.json"),
        list(rel = "parent", href = "../catalog.json"),
        list(rel = "self",   href = paste0(cfg$stac_base, "/", cfg$version, "/collection.json"))),
      child)))
}

#' Root Catalog node
#'
#' @param cfg config from [stac_cfg()]
#' @return STAC Catalog as a list
#' @export
#' @concept stac
stac_root_catalog <- function(cfg) {
  list(
    type         = "Catalog",
    stac_version = "1.0.0",
    id           = "marinesensitivity",
    title        = "MarineSensitivity STAC Catalog",
    description  = paste0(
      "Discovery + provenance catalog for MarineSensitivity species distribution ",
      "model products, using the sdm STAC extension."),
    links = list(
      list(rel = "root",  href = "./catalog.json"),
      list(rel = "self",  href = paste0(cfg$stac_base, "/catalog.json")),
      list(rel = "child", href = paste0("./", cfg$version, "/collection.json"))))
}

#' Build the full static STAC catalog for a version
#'
#' Connects to the version's SDM DuckDB, emits a Collection + model-surface Item
#' per dataset, optionally adds NCCOS seasonal Items from `nc_models.csv`, and
#' writes the tree (root Catalog -> version Collection -> dataset Collections ->
#' Items) under `dir_out`.
#'
#' @param version data version (default "v7")
#' @param dir_out output directory for the catalog tree
#' @param cfg config from [stac_cfg()] (defaults to `stac_cfg(version)`)
#' @param nc_csv optional path to `nc_models.csv` for seasonal Items
#' @param con optional open DBI connection (else opens read-only via [sdm_db_con()])
#' @return invisibly, the path to the root `catalog.json`
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @export
#' @concept stac
stac_build <- function(version = "v7", dir_out = NULL, cfg = NULL,
                       nc_csv = NULL, con = NULL) {
  if (is.null(cfg))     cfg     <- stac_cfg(version)
  if (is.null(dir_out)) dir_out <- file.path(tempdir(), paste0("stac_", version))
  close_con <- FALSE
  if (is.null(con)) { con <- sdm_db_con(version, read_only = TRUE); close_con <- TRUE }
  on.exit(if (close_con) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  datasets <- DBI::dbGetQuery(con, "SELECT * FROM dataset ORDER BY sort_order, ds_key")

  # NCCOS seasonal source keys (from csv; not in the DuckDB in v7)
  d_nc <- NULL; nc_keys <- character()
  if (!is.null(nc_csv) && file.exists(nc_csv)) {
    d_nc    <- utils::read.csv(nc_csv, stringsAsFactors = FALSE)
    nc_keys <- unique(d_nc$ds_key)
  }

  # root + version collection (children = all dataset keys)
  .stac_write(stac_root_catalog(cfg), file.path(dir_out, "catalog.json"))
  .stac_write(stac_version_collection(c(datasets$ds_key, nc_keys), cfg),
              file.path(dir_out, version, "collection.json"))

  # per DuckDB dataset: build item, then collection (with item link), then item
  for (i in seq_len(nrow(datasets))) {
    ds     <- datasets[i, ]
    dir_ds <- file.path(dir_out, version, ds$ds_key)
    tps    <- DBI::dbGetQuery(con, glue::glue(
      "SELECT DISTINCT time_period FROM model WHERE ds_key = '{ds$ds_key}'"))$time_period
    item   <- stac_model_cell_item(ds, cfg, time_periods = tps)
    item_f <- paste0(item$id, ".json")
    .stac_write(stac_dataset_collection(ds, cfg, item_files = item_f),
                file.path(dir_ds, "collection.json"))
    .stac_write(item, file.path(dir_ds, item_f))
  }

  # NCCOS seasonal: per dataset, build per-species items + a stub Collection
  for (k in nc_keys) {
    d_k     <- d_nc[d_nc$ds_key == k, ]
    dir_ds  <- file.path(dir_out, version, k)
    ds_stub <- data.frame(
      ds_key = k, response_type = "density", temporal_res = "seasonal",
      source_broad = "NOAA NCCOS",
      name_display = paste0("NCCOS seabird density (", k, ")"),
      description = "NCCOS seasonal seabird relative-density surfaces (season-named COGs).",
      year_pub = 2021L, date_obs_beg = NA, date_obs_end = NA,
      date_env_beg = NA, date_env_end = NA, citation = NA, name_short = k,
      stringsAsFactors = FALSE)
    items   <- lapply(unique(d_k$sp_code), function(sp)
      stac_season_cog_item(d_k[d_k$sp_code == sp, ], cfg))
    item_fs <- vapply(items, function(it) paste0(it$id, ".json"), character(1))
    .stac_write(stac_dataset_collection(ds_stub, cfg, item_files = item_fs),
                file.path(dir_ds, "collection.json"))
    for (it in items) .stac_write(it, file.path(dir_ds, paste0(it$id, ".json")))
  }

  message(glue::glue("STAC catalog written: {file.path(dir_out, 'catalog.json')}"))
  invisible(file.path(dir_out, "catalog.json"))
}
