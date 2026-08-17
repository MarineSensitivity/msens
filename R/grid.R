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

#' Minimal longitude span of a distribution, antimeridian-aware
#'
#' `min(lon)`/`max(lon)` is the wrong extent for anything that crosses the
#' antimeridian. A North Pacific species with cells at 165 E and 155 W has a
#' -180..180 span of ~360 deg -- the whole globe -- so `fitBounds` centres the
#' camera on longitude 0 and the viewer lands off Iceland while the animal lives
#' in the Bering Sea (apps#9). The span that describes it is 165..205.
#'
#' The rule: measure the span in BOTH frames (-180..180 and 0..360) and keep the
#' narrower one. Longitudes are periodic, so the two frames cut the circle at
#' opposite points (0 deg and 180 deg) and a distribution can straddle at most
#' one of them unless it genuinely encircles the globe -- in which case both
#' spans are wide and the -180..180 answer is returned unchanged.
#'
#' The returned `xmax` may exceed 180. That is deliberate and is what MapLibre
#' wants: `map.fitBounds([160, 48, 210, 66])` centres at 185 -> -175 (verified
#' in-browser). Wrapping it back into -180..180 would put west east of east and
#' fit the COMPLEMENT of the intended box.
#'
#' @param lon numeric vector of longitudes in `[-180, 180]` (non-finite dropped)
#' @return `c(xmin, xmax)`; `c(NA, NA)` when nothing is finite
#' @export
#' @concept grid
#' @examples
#' lon_span(c(-179, -160, 170, 178))   # 170 205  (Bering Sea, not the globe)
#' lon_span(c(-30, 0, 20))             # -30 20   (Atlantic; frame unchanged)
lon_span <- function(lon) {
  lon <- lon[is.finite(lon)]
  if (!length(lon)) return(c(NA_real_, NA_real_))
  l360 <- ifelse(lon < 0, lon + 360, lon)
  lon_span_agg(min(lon), max(lon), min(l360), max(l360))
}

#' @rdname lon_span
#'
#' @details
#' `lon_span_agg()` is the same rule applied to aggregates already computed
#' elsewhere -- the form the apps use, because the four `min`/`max` come back
#' from one DuckDB query over millions of cells and must not be pulled into R
#' just to be reduced. Keeping both forms on one implementation is what stops
#' the SQL path and the vector path from drifting apart.
#'
#' @param x0,x1 min and max longitude in the -180..180 frame
#' @param w0,w1 min and max longitude in the 0..360 frame
#' @export
#' @concept grid
lon_span_agg <- function(x0, x1, w0, w1) {
  if (!is.finite(x0) || !is.finite(x1)) return(c(NA_real_, NA_real_))
  if (!is.finite(w0) || !is.finite(w1)) return(c(x0, x1))
  # A range that straddles BOTH cut points is genuinely circumglobal, and the
  # 0..360 frame can still come out a few degrees narrower by pure accident of
  # where its gap falls. Shifting the frame on that margin would report a
  # circumglobal distribution as a 350 deg box starting at 0 -- narrower than the
  # truth and no more useful. Say the whole globe plainly instead, so
  # bbox_spans_globe() can reject it and the caller falls back.
  if ((w1 - w0) < (x1 - x0) && (w1 - w0) < 350) c(w0, w1) else c(x0, x1)
}

#' Does a bounding box span so much longitude that it cannot frame anything?
#'
#' A whole-world extent is a legitimate answer for an ASSET (a global raster of a
#' wraparound range really does run -180..180) and a useless one for a CAMERA: it
#' says "look at everything", which shows the user nothing. Callers use this to
#' reject such a box and fall back to a data-derived extent, the same way they
#' already treat a missing one.
#'
#' @param bb numeric `c(xmin, ymin, xmax, ymax)`, or `NULL`
#' @param max_span widest longitude span still considered informative, degrees
#' @return `TRUE` when `bb` is absent, non-finite, or spans `>= max_span`
#' @export
#' @concept grid
bbox_spans_globe <- function(bb, max_span = 350) {
  if (is.null(bb) || length(bb) != 4L || !all(is.finite(bb))) return(TRUE)
  (bb[3] - bb[1]) >= max_span
}

#' Cell id at a longitude/latitude (inverse of [cell_lonlat()])
#'
#' Pure arithmetic on the grid definition -- no raster read. The cell-id COGs are
#' lookup IMAGES whose band name and frame vary by grid, so resolving a click by
#' reading one is both slower and a source of bugs: selecting the band by name
#' fails (usa05 calls it `r_cellid`, global05 `depth_mean`), and shifting a click
#' to 0-360 lands outside a raster stored in -180..180. The grid registry already
#' defines the mapping exactly.
#'
#' @param lon,lat coordinates in degrees, `lon` in -180..180
#' @param grid grid spec from [grid_spec_for()]
#' @return cell id, or `NA` where the point falls outside the grid
#' @export
#' @concept grid
cell_from_lonlat <- function(lon, lat, grid) {
  # a 0-360 grid stores its own frame, so bring a -180..180 click into it
  if (isTRUE(grid$lon360)) lon <- ifelse(lon < grid$xmin, lon + 360, lon)
  # Row uses ceiling, column floor+1 -- the asymmetry only shows at an exact cell
  # boundary, and it matches GDAL there for `global05`.
  #
  # This is a FALLBACK, not the authority: the published cell-id COG is, and the
  # app asks titiler for it first. Validated against that COG at interior points
  # on both grids -- all agreed. Exactly ON a boundary `usa05` can still differ by
  # one cell, because `r_cellid.tif` is a lookup IMAGE whose pixels are aligned to
  # -180 while the usa05 grid is defined from 141.10 E, so the two disagree about
  # which side of the line a point sits on. A map click delivers floats, never an
  # exact multiple of 0.05, so this does not arise in practice -- and if it ever
  # matters, read the COG.
  ymin <- grid$ymax - grid$nr * grid$resy
  xmax <- grid$xmin + grid$nc * grid$resx
  col  <- floor((lon - grid$xmin) / grid$resx) + 1
  row  <- ceiling((grid$ymax - lat) / grid$resy)
  # the top edge is row 1, not row 0; every other row already lands correctly
  row  <- ifelse(row == 0 & lat <= grid$ymax, 1, row)
  # validity is a question about the COORDINATES, not about clamped indices --
  # clamping first made a point north of the grid (lat 89 on a 82.6 N grid)
  # report as row 1 instead of NA
  ok <- lon >= grid$xmin & lon < xmax & lat <= grid$ymax & lat > ymin &
        col >= 1 & col <= grid$nc & row >= 1 & row <= grid$nr
  ifelse(ok, (row - 1) * grid$nc + col, NA_real_)
}

#' Public URL of a grid's cell-id COG
#'
#' The cell-id lookup published beside the release data, so a client can resolve
#' "which cell is at this point?" through the same titiler `/cog/point` call it
#' uses for the layer value -- no local raster, and the id comes from the same
#' authority the tiles do. Written INT4U with **no overviews**: cell ids are
#' categorical, and a resampled pyramid would average them into ids that do not
#' exist.
#'
#' @param grid_id `usa05` or `global05`
#' @param base atlas base URL from [atlas_base_url()]
#' @return an https URL
#' @export
#' @concept grid
grid_cellid_url <- function(grid_id, base = atlas_base_url())
  sprintf("%s/grid/%s/cellid.tif", base, grid_id)
