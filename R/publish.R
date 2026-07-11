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
  # the app filters this per-dataset archive to ONE mdl_key, so EVERY feature must
  # survive at EVERY zoom — never --drop-densest / --coalesce (they drop or merge
  # species, so a selected range vanishes at low zoom). Keep all features + tame
  # size with aggressive geometry --simplification and a modest maxzoom (ranges are
  # coarse; higher zooms overzoom cleanly). Only carry the id attributes.
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
