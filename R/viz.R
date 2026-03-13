
#' Basemap
#'
#' Basemap with Esri Ocean Basemap
#'
#' @param base_opacity numeric between 0 and 1 (default=0.5)
#'
#' @return \link[leaflet]{leaflet} map object with Esri.OceanBasemap
#' @import leaflet
#' @export
#' @concept viz
#'
#' @examples
#' ms_basemap()
ms_basemap <- function(base_opacity = 0.5){

  leaflet::leaflet() |>
    # add base: blue bathymetry and light brown/green topography
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = leaflet::providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = base_opacity)) |>
    # add reference: placename labels and borders
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = leaflet::providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = base_opacity))
}


#' Add a PMTile polygon fill layer to a map
#'
#' Adds a PMTiles source and fill layer with match_expr coloring. Works with
#' both initial map widgets and mapboxgl_proxy() updates.
#'
#' @param m map or map_proxy
#' @param url PMTiles URL
#' @param source_layer source layer name
#' @param col_key key column name in PMTiles features
#' @param d data frame with col_key and col_value columns (for match_expr)
#' @param col_value score column for continuous coloring (or NULL)
#' @param colors named character vector of key->color (for categorical; or NULL)
#' @param filter_keys character vector of keys to include
#' @param id fill layer id (default: "main_fill")
#' @param source_id PMTiles source id (default: "main_src")
#' @param n_colors integer; number of color steps (default: 11)
#' @param palette character; RColorBrewer palette name (default: "Spectral")
#' @param reverse_palette logical; reverse palette (default: TRUE)
#' @param fill_opacity numeric (default: 0.7)
#' @param outline_color outline color for a companion line layer (NULL to skip)
#' @param outline_width outline width (default: 1)
#' @param tooltip passed to add_fill_layer
#' @param popup passed to add_fill_layer
#' @param hover_options passed to add_fill_layer
#' @param before_id layer to insert before
#' @return map with legend_meta attribute (list with rng, colors, categorical)
#' @importFrom mapgl add_pmtiles_source add_fill_layer add_line_layer match_expr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @export
#' @concept viz
add_pmfill <- function(
    m, url, source_layer, col_key,
    d = NULL, col_value = NULL, colors = NULL, filter_keys = NULL,
    id = "main_fill", source_id = "main_src",
    n_colors = 11, palette = "Spectral", reverse_palette = TRUE,
    fill_opacity = 0.7,
    outline_color = NULL, outline_width = 1,
    tooltip = NULL, popup = NULL, hover_options = NULL,
    before_id = NULL) {

  # determine keys
  keys <- filter_keys %||% (if (!is.null(d)) d[[col_key]])
  fill_filter <- if (!is.null(keys)) c("in", col_key, keys)

  # compute fill_color expression ----
  if (!is.null(col_value) && !is.null(d)) {
    # continuous: bin scores into color ramp
    pal_colors <- RColorBrewer::brewer.pal(max(n_colors, 3), palette)
    if (reverse_palette) pal_colors <- rev(pal_colors)
    ramp <- grDevices::colorRampPalette(pal_colors)(n_colors)
    vals <- d[[col_value]]
    rng  <- range(vals, na.rm = TRUE)
    brks <- seq(rng[1], rng[2], length.out = n_colors)
    bins <- findInterval(vals, brks, all.inside = TRUE)
    key_colors <- stats::setNames(ramp[bins], d[[col_key]])[keys]
    fc <- mapgl::match_expr(
      column  = col_key,
      values  = unname(keys),
      stops   = unname(key_colors),
      default = "#cccccc")
    legend_meta <- list(rng = rng, colors = ramp, categorical = FALSE)
  } else if (!is.null(colors)) {
    # categorical: colors provided directly
    if (!is.null(names(colors))) {
      key_colors <- colors[keys]
    } else {
      key_colors <- stats::setNames(colors, keys)
    }
    fc <- mapgl::match_expr(
      column  = col_key,
      values  = unname(keys),
      stops   = unname(key_colors),
      default = "#cccccc")
    legend_meta <- list(
      colors      = unname(key_colors),
      categorical = TRUE)
  } else {
    stop("must provide either col_value + d, or colors")
  }

  # add source + fill layer
  m <- m |>
    mapgl::add_pmtiles_source(id = source_id, url = url) |>
    mapgl::add_fill_layer(
      id            = id,
      source        = source_id,
      source_layer  = source_layer,
      fill_color    = fc,
      fill_opacity  = fill_opacity,
      filter        = fill_filter,
      before_id     = before_id,
      tooltip       = tooltip,
      popup         = popup,
      hover_options = hover_options)

  # optional companion outline
  if (!is.null(outline_color)) {
    ln_id <- sub("_fill$", "_ln", id)
    if (ln_id == id) ln_id <- paste0(id, "_ln")
    m <- m |>
      mapgl::add_line_layer(
        id           = ln_id,
        source       = source_id,
        source_layer = source_layer,
        line_color   = outline_color,
        line_opacity = 1,
        line_width   = outline_width,
        filter       = fill_filter)
  }

  # attach legend metadata for downstream legend helpers
  attr(m, "legend_meta") <- legend_meta
  m
}


#' Add PMTile outline line layers to a map
#'
#' @param m map or map_proxy
#' @param outlines list of specs, each with: url, source_layer, and optionally
#'   id, source_id, line_color, line_width, line_opacity, filter, before_id
#' @return map (pipeable)
#' @importFrom mapgl add_pmtiles_source add_line_layer
#' @export
#' @concept viz
add_pmline <- function(m, outlines) {
  for (i in seq_along(outlines)) {
    ol     <- outlines[[i]]
    src_id <- ol$source_id %||% paste0("outline_src_", i)
    ln_id  <- ol$id        %||% paste0("outline_ln_", i)
    m <- m |>
      mapgl::add_pmtiles_source(id = src_id, url = ol$url) |>
      mapgl::add_line_layer(
        id           = ln_id,
        source       = src_id,
        source_layer = ol$source_layer,
        line_color   = ol$line_color   %||% "gray",
        line_opacity = ol$line_opacity %||% 1,
        line_width   = ol$line_width   %||% 1,
        filter       = ol$filter,
        before_id    = ol$before_id)
  }
  m
}


#' Add symbol label layers from sf point data
#'
#' Auto-detects text_anchor, text_justify, text_offset_right, text_offset_down
#' columns in the source sf for per-feature label placement.
#'
#' @param m map or map_proxy
#' @param labels list of label specs, each with: source (sf), text_field, and
#'   optionally id, text_color, text_size, text_font, text_halo_color,
#'   text_halo_width, text_halo_blur, text_line_height, text_allow_overlap,
#'   filter, text_anchor, text_justify, text_offset
#' @return map (pipeable)
#' @importFrom mapgl add_symbol_layer get_column
#' @export
#' @concept viz
add_pmlabel <- function(m, labels) {
  for (i in seq_along(labels)) {
    lbl    <- labels[[i]]
    lbl_id <- lbl$id %||% paste0("label_", i)
    src    <- lbl$source

    has_anchor <- "text_anchor" %in% names(src)
    has_offset <- all(c("text_offset_right", "text_offset_down") %in% names(src))

    sym_args <- list(
      map                = m,
      id                 = lbl_id,
      source             = src,
      text_field         = mapgl::get_column(lbl$text_field),
      text_color         = lbl$text_color         %||% "white",
      text_allow_overlap = lbl$text_allow_overlap  %||% TRUE,
      filter             = lbl$filter)

    # optional text styling args
    for (nm in c("text_size", "text_font", "text_halo_color",
                 "text_halo_width", "text_halo_blur", "text_line_height"))
      sym_args[[nm]] <- lbl[[nm]]

    # per-feature anchor/justify from sf columns or explicit value
    sym_args$text_anchor  <- lbl$text_anchor  %||%
      if (has_anchor) mapgl::get_column("text_anchor")
    sym_args$text_justify <- lbl$text_justify %||%
      if ("text_justify" %in% names(src)) mapgl::get_column("text_justify")

    # per-feature offset from sf columns or explicit value
    if (!is.null(lbl$text_offset)) {
      sym_args$text_offset <- lbl$text_offset
    } else if (has_offset) {
      sym_args$text_offset <- list(
        mapgl::get_column("text_offset_right"),
        mapgl::get_column("text_offset_down"))
    }

    sym_args <- sym_args[!vapply(sym_args, is.null, logical(1))]
    m <- do.call(mapgl::add_symbol_layer, sym_args)
  }
  m
}


#' Add a raster cell layer to a map
#'
#' Adds an image source from a terra SpatRaster and a raster layer. Works with
#' both initial map widgets and mapboxgl_proxy() updates.
#'
#' @param m map or map_proxy
#' @param r terra SpatRaster
#' @param colors character vector of colors
#' @param id layer id (default: "r_lyr")
#' @param source_id source id (default: "r_src")
#' @param raster_opacity numeric (default: 0.8)
#' @param raster_resampling character (default: "nearest")
#' @param before_id layer to insert before
#' @param ... additional args to add_raster_layer
#' @return map (pipeable)
#' @importFrom mapgl add_image_source add_raster_layer
#' @export
#' @concept viz
add_cells <- function(
    m, r, colors,
    id = "r_lyr", source_id = "r_src",
    raster_opacity = 0.8, raster_resampling = "nearest",
    before_id = NULL, ...) {
  m |>
    mapgl::add_image_source(id = source_id, data = r, colors = colors) |>
    mapgl::add_raster_layer(
      id                 = id,
      source             = source_id,
      raster_opacity     = raster_opacity,
      raster_resampling  = raster_resampling,
      before_id          = before_id, ...)
}


#' Map raster cells with outlines and labels
#'
#' Convenience constructor that composes add_cells(), add_pmline(), and
#' add_pmlabel() into a complete map with legend and scale control.
#'
#' @param r terra SpatRaster
#' @param colors character vector of colors (or NULL to auto-generate)
#' @param base_map a mapboxgl or maplibre map object (or NULL for default dark)
#' @param bounds sf or bbox object to fit map bounds to
#' @param raster_opacity numeric (default: 0.9)
#' @param raster_resampling character (default: "nearest")
#' @param n_colors integer; number of color steps (default: 11)
#' @param palette character; RColorBrewer palette name (default: "Spectral")
#' @param reverse_palette logical; reverse palette (default: TRUE)
#' @param legend_title character (default: "Score")
#' @param legend_position character (default: "bottom-left")
#' @param legend_values numeric range for legend (or NULL for auto)
#' @param pmtiles_outlines list of outline specs for add_pmline()
#' @param labels list of label specs for add_pmlabel()
#' @return a mapboxgl htmlwidget
#' @importFrom mapgl mapboxgl mapbox_style fit_bounds add_legend
#'   add_scale_control
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @export
#' @concept viz
map_cells <- function(
    r, colors = NULL, base_map = NULL, bounds = NULL,
    raster_opacity = 0.9, raster_resampling = "nearest",
    n_colors = 11, palette = "Spectral", reverse_palette = TRUE,
    legend_title = "Score", legend_position = "bottom-left",
    legend_values = NULL, pmtiles_outlines = NULL, labels = NULL) {

  if (is.null(colors)) {
    pal <- RColorBrewer::brewer.pal(max(n_colors, 3), palette)
    if (reverse_palette) pal <- rev(pal)
    colors <- grDevices::colorRampPalette(pal)(n_colors)
  }
  if (is.null(legend_values))
    legend_values <- terra::minmax(r) |> as.numeric()

  m <- base_map %||% mapgl::mapboxgl(
    style = mapgl::mapbox_style("dark"), projection = "globe")
  if (!is.null(bounds))
    m <- m |> mapgl::fit_bounds(bbox = bounds)

  m <- m |> add_cells(
    r, colors,
    raster_opacity     = raster_opacity,
    raster_resampling  = raster_resampling)

  if (!is.null(pmtiles_outlines))
    m <- m |> add_pmline(pmtiles_outlines)
  if (!is.null(labels))
    m <- m |> add_pmlabel(labels)

  m |>
    mapgl::add_legend(
      legend_title,
      values   = legend_values,
      colors   = colors,
      position = legend_position) |>
    mapgl::add_scale_control(position = "bottom-right")
}


#' Map data on PMTile vector tiles
#'
#' Create a mapboxgl/maplibre map with polygon fill colors driven by a data
#' frame, using PMTile vector tile sources. Data is joined to features at render
#' time via match expressions, so PMTiles don't need to be regenerated.
#'
#' Internally uses add_pmfill(), add_pmline(), and add_pmlabel().
#'
#' @param d data frame with a key column and either a score or color column
#' @param col_key character; name of the key column in `d`
#' @param col_value character; name of the score column for continuous coloring
#'   (or NULL)
#' @param colors character vector of colors per row (or named by key), for
#'   categorical coloring (or NULL)
#' @param pmtiles_url character; URL to the PMTiles file
#' @param source_layer character; source layer name within the PMTiles
#' @param base_map a mapboxgl or maplibre map object; if NULL creates
#'   mapboxgl(dark, globe)
#' @param bounds sf or bbox object to fit map bounds to
#' @param filter_keys character vector of keys to show (subset of d)
#' @param n_colors integer; number of color steps for continuous mode
#'   (default: 11)
#' @param palette character; RColorBrewer palette name (default: "Spectral")
#' @param reverse_palette logical; reverse palette so red=high (default: TRUE)
#' @param fill_opacity numeric (default: 0.7)
#' @param outline_color character (default: "white")
#' @param outline_width numeric (default: 1)
#' @param legend_title character (default: "Score")
#' @param legend_position character (default: "bottom-left")
#' @param categorical logical; use categorical legend (default: FALSE)
#' @param legend_labels character vector of labels for categorical legend
#' @param tooltip passed to add_fill_layer
#' @param popup passed to add_fill_layer
#' @param hover_options passed to add_fill_layer
#' @param pmtiles_outlines list of outline layer specs for add_pmline()
#' @param labels list of label specs for add_pmlabel()
#'
#' @return a mapboxgl/maplibre htmlwidget (pipeable for additional layers)
#' @importFrom mapgl mapboxgl mapbox_style fit_bounds add_legend
#'   add_categorical_legend add_scale_control
#' @export
#' @concept viz
map_pmtiles <- function(
    d,
    col_key,
    col_value        = NULL,
    colors           = NULL,
    pmtiles_url,
    source_layer,
    base_map         = NULL,
    bounds           = NULL,
    filter_keys      = NULL,
    n_colors         = 11,
    palette          = "Spectral",
    reverse_palette  = TRUE,
    fill_opacity     = 0.7,
    outline_color    = "white",
    outline_width    = 1,
    legend_title     = "Score",
    legend_position  = "bottom-left",
    categorical      = FALSE,
    legend_labels    = NULL,
    tooltip          = NULL,
    popup            = NULL,
    hover_options    = NULL,
    pmtiles_outlines = NULL,
    labels           = NULL) {

  stopifnot(
    !is.null(col_value) || !is.null(colors),
    col_key %in% names(d))

  # base map ----
  m <- base_map %||% mapgl::mapboxgl(
    style      = mapgl::mapbox_style("dark"),
    projection = "globe")
  if (!is.null(bounds))
    m <- m |> mapgl::fit_bounds(bbox = bounds)

  # fill layer via add_pmfill ----
  m <- m |> add_pmfill(
    url             = pmtiles_url,
    source_layer    = source_layer,
    col_key         = col_key,
    d               = d,
    col_value       = col_value,
    colors          = colors,
    filter_keys     = filter_keys,
    n_colors        = n_colors,
    palette         = palette,
    reverse_palette = reverse_palette,
    fill_opacity    = fill_opacity,
    outline_color   = outline_color,
    outline_width   = outline_width,
    tooltip         = tooltip,
    popup           = popup,
    hover_options   = hover_options)

  legend_meta <- attr(m, "legend_meta")

  # outline layers ----
  if (!is.null(pmtiles_outlines))
    m <- m |> add_pmline(pmtiles_outlines)

  # label layers ----
  if (!is.null(labels))
    m <- m |> add_pmlabel(labels)

  # legend ----
  if (categorical) {
    keys <- filter_keys %||% d[[col_key]]
    lbls <- if (!is.null(legend_labels)) legend_labels else keys
    m <- m |>
      mapgl::add_categorical_legend(
        legend_title = legend_title,
        values       = lbls,
        colors       = legend_meta$colors,
        position     = legend_position)
  } else {
    m <- m |>
      mapgl::add_legend(
        legend_title = legend_title,
        values       = legend_meta$rng,
        colors       = legend_meta$colors,
        position     = legend_position)
  }

  m |>
    mapgl::add_scale_control(position = "bottom-right")
}
