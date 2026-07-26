# widget.R — static snapshots of heavyweight htmlwidgets ----
#
# WHY. An interactive htmlwidget serialises its ENTIRE data payload into the
# rendered HTML. Mapping full-resolution ranges or rasters therefore produced
# published pages of 43-53 MB, dominated by a single <script> block (one turtle
# map embedded 26 MB of GeoJSON; a study-area map, 40 MB). GitHub warns above
# 50 MB, and every visitor downloads the whole payload just to look at a picture.
#
# For a figure nobody pans or clicks, a PNG is the honest representation: save
# the widget to a throwaway HTML, screenshot it with headless Chrome, and embed
# the image instead. Pages drop by orders of magnitude and stay readable.
#
# Side benefit: the MapTiler style URL carries an API key, which the embedded
# JSON published verbatim. A screenshot leaves the key out of the HTML entirely.

#' Render an htmlwidget to a static PNG
#'
#' Saves `widget` to a temporary self-contained-free HTML, screenshots it with
#' headless Chrome via `webshot2`, and returns the image path — for use as
#' `knitr::include_graphics(widget_png(m, "figures/map.png"))` in a notebook
#' chunk, in place of printing the widget.
#'
#' Use it for maps whose payload is large and whose interactivity is not the
#' point. Keep printing the widget where panning/zooming genuinely matters.
#'
#' @param widget an htmlwidget (e.g. from `mapgl::maplibre()`, `mapview`, `leaflet`)
#' @param file output PNG path, relative to the notebook (parent dirs created)
#' @param width,height viewport size in CSS pixels
#' @param delay seconds to wait before the screenshot — map tiles and GeoJSON
#'   layers load asynchronously, so too short a delay yields a blank basemap
#' @param zoom device pixel ratio; 2 gives a retina-quality PNG
#' @return `file`, invisibly-safe for `knitr::include_graphics()`
#' @examples
#' \dontrun{
#' knitr::include_graphics(widget_png(m_gap, "figures/gap_turtles.png"))
#' }
#' @export
#' @concept viz
widget_png <- function(widget, file, width = 1200, height = 800,
                       delay = 8, zoom = 2) {
  stopifnot(length(file) == 1L, nzchar(file))
  for (p in c("htmlwidgets", "webshot2"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("widget_png() needs the '", p, "' package", call. = FALSE)

  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)

  # selfcontained = FALSE keeps this fast (no pandoc base64 pass) — the HTML is
  # a scratch file that only headless Chrome ever opens.
  tmp_dir  <- tempfile("widget_png_"); dir.create(tmp_dir)
  tmp_html <- file.path(tmp_dir, "widget.html")
  htmlwidgets::saveWidget(widget, tmp_html, selfcontained = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  webshot2::webshot(tmp_html, file, vwidth = width, vheight = height,
                    delay = delay, zoom = zoom)
  if (!file.exists(file))
    stop("widget_png(): webshot2 produced no image at ", file, call. = FALSE)
  file
}
