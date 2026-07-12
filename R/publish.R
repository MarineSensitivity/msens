# publish.R — native-format publishing of per-model SDM surfaces
#
# Phase 4b of the v8 marine-atlas: alongside the merged per-cell surfaces served
# by the SQL titiler, each *raw input* model is published in a cloud-native,
# pyramided format so the species app can overlay the whole (often global) range
# a source contributes — surfaces far too large (up to ~19M cells) for the
# dense per-cell SQL tiler:
#   - raster sources (AquaMaps)         -> per-model COG with overviews  (publish_cog)
#   - vector sources (IUCN/FWS/NMFS...) -> one PMTiles per dataset        (publish_pmtiles)
# See workflows/publish_native.qmd for the orchestration.

# filesystem/URL-safe slug for the sp_id portion of an mdl_key. Defined at package scope (NOT
# as a closure inside the publish_* helpers) so future/furrr doesn't drag the helper's local
# environment — a per-model sf can be hundreds of MB — into every parallel worker.
.slug_sp <- function(s) gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", s))

#' Grid spec for the global cell-id raster
#'
#' Plain-list description of the global 0.05° [-180,180] cell-id grid, so
#' [publish_cog()] can construct a windowed raster from a model's `cell_id`s
#' without serializing a terra SpatRaster across parallel workers. `cell_id` is
#' the row-major (top-left origin) 1-based cell index of this grid, so a cell's
#' row/col is pure arithmetic (no lookup against the COG).
#'
#' @param r a terra SpatRaster of the cell-id COG (`r_cellid_global.tif`)
#' @return a list with `nc`, `nr`, `xmin`, `ymax`, `resx`, `resy`, `crs`
#' @importFrom terra ncol nrow ext res crs
#' @export
#' @concept publish
grid_spec <- function(r) {
  e <- as.vector(terra::ext(r)); rs <- terra::res(r)
  list(nc = terra::ncol(r), nr = terra::nrow(r),
       xmin = e[["xmin"]], ymax = e[["ymax"]], resx = rs[1], resy = rs[2],
       crs = terra::crs(r))
}

#' Publish one model's cells as a COG
#'
#' Paints a model's `(cell_id, val)` onto the global 0.05° grid, cropped to the
#' cells' bounding box, and writes a Cloud-Optimized GeoTIFF with internal
#' overviews (so a global range renders cheaply at low zoom via titiler `/cog`).
#' Cropping to the data window keeps each COG small (most ranges are regional).
#'
#' @param cell_id integer vector of global row-major cell ids (1-based)
#' @param val numeric vector of values aligned to `cell_id` (e.g. 1-100)
#' @param out_tif output path (`.tif`)
#' @param grid grid spec from [grid_spec()]
#' @param datatype terra datatype (default `"INT1U"`; use `"FLT4S"` for floats)
#' @param nodata value flagged as NoData (default `0`; AquaMaps vals are 1-100)
#' @param overview logical; build internal overviews (default `TRUE`)
#' @return `out_tif` (invisibly), or `NULL` if no finite cells
#' @importFrom terra rast writeRaster
#' @export
#' @concept publish
publish_cog <- function(cell_id, val, out_tif, grid,
                        datatype = "INT1U", nodata = 0, overview = TRUE) {
  ok <- !is.na(cell_id) & !is.na(val) & cell_id >= 1 & cell_id <= grid$nc * grid$nr
  cell_id <- as.double(cell_id[ok]); val <- val[ok]
  if (!length(cell_id)) return(NULL)

  # row/col of each cell in the global grid (row-major, top-left origin)
  row <- ((cell_id - 1) %/% grid$nc) + 1
  col <- ((cell_id - 1) %%  grid$nc) + 1
  r0 <- min(row); r1 <- max(row); c0 <- min(col); c1 <- max(col)
  wnc <- c1 - c0 + 1; wnr <- r1 - r0 + 1

  # geographic extent of the crop window (top-left origin -> y decreases w/ row)
  xmn <- grid$xmin + (c0 - 1) * grid$resx
  xmx <- grid$xmin +  c1      * grid$resx
  ymx <- grid$ymax - (r0 - 1) * grid$resy
  ymn <- grid$ymax -  r1      * grid$resy
  r <- terra::rast(nrows = wnr, ncols = wnc, xmin = xmn, xmax = xmx,
                   ymin = ymn, ymax = ymx, crs = grid$crs)

  # scatter values by LOCAL (windowed) cell index, then write COG + overviews
  loc <- (row - r0) * wnc + (col - c0) + 1
  v <- rep(NA_real_, wnr * wnc); v[loc] <- val
  r[] <- v
  gdal <- c("COMPRESS=DEFLATE", "OVERVIEW_RESAMPLING=NEAREST", "BLOCKSIZE=256")
  if (!overview) gdal <- c(gdal, "OVERVIEWS=NONE")
  terra::writeRaster(r, out_tif, filetype = "COG", datatype = datatype,
                     NAflag = nodata, overwrite = TRUE, gdal = gdal)
  invisible(out_tif)
}

#' Publish vector features as PMTiles (tippecanoe)
#'
#' Writes an sf (or an existing vector file) to PMTiles via tippecanoe, keeping
#' every feature (no dropping) so a source dataset's ranges can be filtered
#' client-side by an attribute (e.g. `mdl_key`). Reprojects to EPSG:4326 first.
#'
#' @param x an sf object, or a path to a vector file tippecanoe can read
#'   (FlatGeobuf / GeoJSON)
#' @param out_pmtiles output path (`.pmtiles`)
#' @param layer tile layer name (the `source_layer` the app references)
#' @param minzoom,maxzoom zoom range (default 0..6; coarse expert ranges don't
#'   need street-level detail, and high zoom on huge global polygons is very slow)
#' @param simplification tippecanoe `--simplification` (default 10; aggressive,
#'   since ranges are coarse — keeps tiles small without dropping species)
#' @param tippecanoe path to the tippecanoe binary (default `"tippecanoe"`)
#' @param extra extra tippecanoe CLI args (character vector)
#' @param quiet suppress tippecanoe stderr (default `TRUE`)
#' @return `out_pmtiles` (invisibly)
#' @importFrom sf st_write st_transform st_crs
#' @export
#' @concept publish
publish_pmtiles <- function(x, out_pmtiles, layer,
                            minzoom = 0, maxzoom = 6, simplification = 20,
                            tippecanoe = "tippecanoe", extra = character(0),
                            quiet = TRUE) {
  if (inherits(x, "sf")) {
    if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 4326
    epsg <- sf::st_crs(x)$epsg
    if (is.na(epsg) || !identical(as.integer(epsg), 4326L))
      x <- sf::st_transform(x, 4326)
    src <- tempfile(fileext = ".fgb")
    sf::st_write(x, src, quiet = TRUE, delete_dsn = TRUE)
    on.exit(unlink(src), add = TRUE)
  } else {
    stopifnot(file.exists(x)); src <- x
  }
  # keep EVERY feature at EVERY zoom — never --drop-densest / --coalesce (they drop or
  # merge features, so a range vanishes at low zoom). For a per-MODEL file (one species;
  # see publish_pmtiles_models) every feature is the same range, so low-zoom tiles stay
  # tiny; tame any complex single range with --simplification + a modest maxzoom (ranges
  # are coarse; higher zooms overzoom cleanly). Only carry the id attributes.
  args <- c("-o", out_pmtiles, "-l", layer,
            "-Z", minzoom, "-z", maxzoom, "--simplification", simplification,
            "--no-tile-size-limit", "--no-feature-limit",
            "-y", "mdl_key", "-y", "ds_key", "--force",
            extra, src)
  st <- system2(tippecanoe, as.character(args),
                stdout = if (quiet) FALSE else "", stderr = if (quiet) FALSE else "")
  if (!identical(st, 0L)) stop("tippecanoe failed (status ", st, ") for ", out_pmtiles)
  invisible(out_pmtiles)
}

#' Publish per-model PMTiles — one file per `mdl_key`
#'
#' Splits an sf carrying a `mdl_key` column into **one PMTiles per model**, written to
#' `{dir_out}/{slug(sp_id)}.pmtiles` (`sp_id` = `mdl_key` after the first `|`). Each file
#' holds a single model, so the species app needs no client-side filter and — crucially —
#' low-zoom tiles never collide across species. Packing thousands of overlapping global
#' ranges into one per-dataset archive overflows the z0–z2 world tiles past maplibre's
#' per-tile budget, so it silently drops them (a selected range shows "no polygon at all"
#' at global zoom); a per-model file tiles to a few KB and always renders. Mirrors the
#' per-model AquaMaps COGs. Parallel (`furrr`) + resumable (skips existing unless `redo`).
#'
#' @param x an sf with a character `mdl_key` column (features may repeat a key)
#' @param dir_out output directory (created); one `.pmtiles` per model
#' @param layer tile layer name (the app's `source_layer`)
#' @param workers parallel workers (default `max(1, detectCores() - 2)`)
#' @param redo rebuild files that already exist (default `FALSE`)
#' @param minzoom,maxzoom,simplification passed to [publish_pmtiles()]
#' @return a tibble `(mdl_key, sp_id, file, mb)`, one row per model built or found
#' @importFrom sf st_is_empty
#' @importFrom fs dir_create file_exists file_size path
#' @export
#' @concept publish
publish_pmtiles_models <- function(x, dir_out, layer, workers = NULL, redo = FALSE,
                                   minzoom = 0, maxzoom = 6, simplification = 10) {
  stopifnot(inherits(x, "sf"), "mdl_key" %in% names(x))
  fs::dir_create(dir_out)
  x$mdl_key <- as.character(x$mdl_key)
  if (!"ds_key" %in% names(x)) x$ds_key <- sub("\\|.*$", "", x$mdl_key)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  parts <- split(x, x$mdl_key)                                   # one small sf per model
  if (is.null(workers)) workers <- max(1L, parallel::detectCores() - 2L)

  build_one <- function(part) {
    key   <- part$mdl_key[1]
    sp_id <- sub("^[^|]*\\|", "", key)                           # everything after the first '|'
    file  <- fs::path(dir_out, paste0(.slug_sp(sp_id), ".pmtiles"))
    if (!fs::file_exists(file) || redo)
      tryCatch(publish_pmtiles(part, file, layer = layer, minzoom = minzoom,
                               maxzoom = maxzoom, simplification = simplification),
               error = function(e) NULL)
    if (!fs::file_exists(file)) return(NULL)
    tibble::tibble(mdl_key = key, sp_id = sp_id, file = as.character(file),
                   mb = round(as.numeric(fs::file_size(file)) / 1e6, 3))
  }

  if (workers > 1L && requireNamespace("furrr", quietly = TRUE)) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
    res <- furrr::future_map(parts, build_one,
                             .options = furrr::furrr_options(seed = TRUE))
  } else {
    res <- lapply(parts, build_one)
  }
  do.call(rbind, res)
}

#' Publish per-model PMTiles from an indexed GeoPackage
#'
#' Like [publish_pmtiles_models()] but reads each model's features **on demand** from an
#' indexed GeoPackage (`SELECT ... WHERE mdl_key = ...`) instead of a pre-loaded sf — so a
#' multi-GB source (full-resolution IUCN ranges are ~7 GB) never loads into memory at once,
#' and the per-model query is an index hit. Parallel (`furrr`) + resumable.
#'
#' @param gpkg path to a GeoPackage whose `layer` has a character `mdl_key` column
#'   (index it on `mdl_key` for speed: `CREATE INDEX ... ON layer(mdl_key)`)
#' @param layer gpkg layer name (also the tile `source_layer`)
#' @param keys character `mdl_key`s to publish (each -> one file)
#' @param dir_out output directory (created); one `{slug(sp_id)}.pmtiles` per key
#' @param workers,redo,minzoom,maxzoom,simplification as in [publish_pmtiles_models()]
#' @return a tibble `(mdl_key, sp_id, file, mb)`, one row per model built or found
#' @importFrom sf st_read st_zm st_is_empty
#' @importFrom fs dir_create file_exists file_size path
#' @export
#' @concept publish
publish_pmtiles_from_gpkg <- function(gpkg, layer, keys, dir_out, workers = NULL, redo = FALSE,
                                      minzoom = 0, maxzoom = 6, simplification = 10) {
  stopifnot(file.exists(gpkg))
  fs::dir_create(dir_out)
  keys <- unique(as.character(keys))
  if (is.null(workers)) workers <- max(1L, parallel::detectCores() - 2L)

  build_one <- function(key) {
    sp_id <- sub("^[^|]*\\|", "", key)
    file  <- fs::path(dir_out, paste0(.slug_sp(sp_id), ".pmtiles"))
    made  <- function() tibble::tibble(mdl_key = key, sp_id = sp_id, file = as.character(file),
                                       mb = round(as.numeric(fs::file_size(file)) / 1e6, 3))
    if (fs::file_exists(file) && !redo) return(made())
    part <- tryCatch(sf::st_read(gpkg, quiet = TRUE, query = sprintf(
      "SELECT * FROM \"%s\" WHERE mdl_key = '%s'", layer, gsub("'", "''", key))),
      error = function(e) NULL)
    if (is.null(part) || !nrow(part)) return(NULL)
    part$mdl_key <- key
    if (!"ds_key" %in% names(part)) part$ds_key <- sub("\\|.*$", "", key)
    part <- sf::st_zm(part, drop = TRUE)
    part <- part[!sf::st_is_empty(part), , drop = FALSE]
    if (!nrow(part)) return(NULL)
    tryCatch(publish_pmtiles(part, file, layer = layer, minzoom = minzoom,
                             maxzoom = maxzoom, simplification = simplification),
             error = function(e) NULL)
    if (fs::file_exists(file)) made() else NULL
  }

  if (workers > 1L && requireNamespace("furrr", quietly = TRUE)) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
    res <- furrr::future_map(keys, build_one, .options = furrr::furrr_options(seed = TRUE))
  } else {
    res <- lapply(keys, build_one)
  }
  do.call(rbind, res)
}
