# grid.R — the GRID registry
#
# Two incompatible cell grids exist across MST releases, and `cell_id` means a
# different place on each. Conflating them does not error — it silently scatters
# a model's cells across the wrong ocean — so every cell_id, content hash and COG
# in the multi-version atlas carries its `grid_id`.
#
#   usa05    v1-v7   3103 x 2006, 0.05 deg, xmin 141.10, ymax 82.60   ncell  6,224,618
#            LONGITUDE IS 0-360: the frame runs 141.10 E eastward ACROSS the
#            antimeridian to 296.25 (= 63.75 W), so Alaska/the Aleutians and the
#            US East Coast sit in one contiguous raster. Verified by inverting
#            sampled cell ids: 3418971 -> (-90.525, 27.525) Gulf of Mexico,
#            1909113 -> (179.475, 51.825) Aleutians.
#   global05 v8+     7200 x 3600, 0.05 deg, xmin -180, ymax 90        ncell 25,920,000
#            Ordinary -180..180.
#
# NOTE the cell-id COGs (`r_cellid.tif`, `r_cellid_global.tif`) are LOOKUP IMAGES
# — each pixel's VALUE is the cell_id covering it — not positional indexes. The
# titiler factory reads a pixel and looks up `vmap[cell_id]`, so its raster may
# use any frame that carries the right ids (r_cellid.tif is 7200 x 2006 in
# -180..180, holding 662,075 usa05 ids). Do NOT infer a grid's geometry from its
# cell-id COG: use this registry. `publish_cog()`, which paints cells by
# arithmetic rather than lookup, needs the real geometry below.

.GRIDS <- list(
  usa05 = list(
    grid_id = "usa05", nc = 3103L, nr = 2006L,
    xmin = 141.10, ymax = 82.60, resx = 0.05, resy = 0.05,
    crs = "EPSG:4326", lon360 = TRUE,
    cellid_tif = "r_cellid.tif",
    label = "US study area, 0.05 deg (0-360 frame)"),
  global05 = list(
    grid_id = "global05", nc = 7200L, nr = 3600L,
    xmin = -180, ymax = 90, resx = 0.05, resy = 0.05,
    crs = "EPSG:4326", lon360 = FALSE,
    cellid_tif = "r_cellid_global.tif",
    label = "Global, 0.05 deg [-180,180]"))

# which grid each release's cell_id indexes
.GRID_VER <- c(v1 = "usa05", v2 = "usa05", v3 = "usa05", v4 = "usa05",
               v4b = "usa05", v5 = "usa05", v6 = "usa05", v7 = "usa05",
               v8 = "global05")

#' The grid registry
#'
#' @return a data frame, one row per known grid
#' @export
#' @concept grid
grid_registry <- function()
  do.call(rbind, lapply(.GRIDS, function(g)
    data.frame(g[c("grid_id", "nc", "nr", "xmin", "ymax", "resx", "resy",
                   "crs", "lon360", "cellid_tif", "label")],
               ncell = as.numeric(g$nc) * g$nr, stringsAsFactors = FALSE)))

#' The grid a release's `cell_id` values index
#'
#' Errors on an unknown version rather than assuming the current grid — assuming
#' is how v7 ids get painted on v8's grid and land in the wrong ocean.
#'
#' @param ver version label, e.g. `"v6"`
#' @return a `grid_id`
#' @export
#' @concept grid
grid_for_ver <- function(ver) {
  ver <- as.character(ver)[1]
  if (is.na(g <- .GRID_VER[ver]) || is.null(g))
    stop(sprintf("no grid registered for version '%s'; known: %s", ver,
                 paste(names(.GRID_VER), collapse = ", ")), call. = FALSE)
  unname(g)
}

#' Grid spec by id, for [publish_cog()]
#'
#' Same plain-list shape [grid_spec()] returns from a raster, plus `grid_id` and
#' `lon360`. When the grid's cell-id COG is on hand its geometry is read from the
#' file (authoritative, and keeps already-published v8 COGs bit-comparable, since
#' the real raster carries ~6e-6 deg of float drift from the nominal values); the
#' registry values are the offline fallback and are asserted against the raster.
#'
#' @param grid_id one of [grid_registry()]`$grid_id`
#' @param cellid_tif optional path to that grid's cell-id COG
#' @return a list with `nc`, `nr`, `xmin`, `ymax`, `resx`, `resy`, `crs`,
#'   `grid_id`, `lon360`
#' @importFrom terra rast
#' @export
#' @concept grid
grid_spec_for <- function(grid_id, cellid_tif = NULL) {
  g <- .GRIDS[[grid_id]]
  if (is.null(g))
    stop(sprintf("unknown grid_id '%s'; known: %s", grid_id,
                 paste(names(.GRIDS), collapse = ", ")), call. = FALSE)
  out <- g[c("nc", "nr", "xmin", "ymax", "resx", "resy", "crs")]
  if (!is.null(cellid_tif) && file.exists(cellid_tif)) {
    s <- grid_spec(terra::rast(cellid_tif))
    # a cell-id COG may legitimately use a different FRAME (it is a lookup image),
    # so only adopt its geometry when the shape actually matches this grid
    if (identical(as.integer(s$nc), g$nc) && identical(as.integer(s$nr), g$nr))
      out <- s
  }
  c(out, list(grid_id = g$grid_id, lon360 = g$lon360))
}

#' Longitude/latitude of cell centers on a grid
#'
#' Row-major, 1-based, top-left origin — the same arithmetic [publish_cog()] uses.
#' On a `lon360` grid the returned longitude is wrapped to `[-180, 180)`.
#'
#' @param cell_id integer vector of cell ids
#' @param grid grid spec from [grid_spec_for()]
#' @param wrap wrap a 0-360 grid onto -180..180. TRUE (default) for plotting a
#'   point; FALSE to keep the grid's own frame, which an EXTENT needs -- a
#'   wrapped antimeridian-crossing grid yields a whole-globe bounding box
#' @return a data frame with `lon`, `lat`
#' @export
#' @concept grid
cell_lonlat <- function(cell_id, grid, wrap = TRUE) {
  cell_id <- as.double(cell_id)
  row <- ((cell_id - 1) %/% grid$nc) + 1
  col <- ((cell_id - 1) %%  grid$nc) + 1
  lon <- grid$xmin + (col - 0.5) * grid$resx
  # wrap = FALSE keeps a 0-360 grid in its OWN frame. Wrapping is right for
  # plotting a point, but wrong for an EXTENT: usa05 runs 141.10 E across the
  # antimeridian, so a wrapped Alaska spans -180..180 and its bounding box comes
  # out as the whole globe instead of the Bering Sea.
  if (isTRUE(grid$lon360) && isTRUE(wrap)) lon <- ifelse(lon >= 180, lon - 360, lon)
  data.frame(lon = lon, lat = grid$ymax - (row - 0.5) * grid$resy)
}
