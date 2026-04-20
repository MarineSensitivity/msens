
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


#' Add an msens cell tile layer to a mapgl map
#'
#' Viewport-driven XYZ tile alternative to [add_cells()]: the browser only
#' fetches the tiles visible in the current viewport from the msens TiTiler
#' factory, rather than shipping the whole raster as a base64-encoded image.
#'
#' @param m map or map_proxy
#' @param tile_url character(1) XYZ tile URL template, typically from
#'   [cell_tile_url()] (must contain `{z}/{x}/{y}` placeholders)
#' @param id layer id (default: "r_lyr")
#' @param source_id source id (default: "r_src")
#' @param tile_size numeric (default: 256)
#' @param raster_opacity numeric (default: 0.8)
#' @param raster_resampling character (default: "nearest")
#' @param before_id layer to insert before
#' @param ... additional args to [mapgl::add_raster_layer()]
#' @return map (pipeable)
#' @importFrom mapgl add_raster_source add_raster_layer
#' @export
#' @concept viz
add_cell_tiles <- function(
    m, tile_url,
    id = "r_lyr", source_id = "r_src",
    tile_size = 256,
    raster_opacity = 0.8, raster_resampling = "nearest",
    before_id = NULL, ...) {
  stopifnot(is.character(tile_url), length(tile_url) == 1)
  m |>
    mapgl::add_raster_source(
      id       = source_id,
      tiles    = tile_url,
      tileSize = tile_size) |>
    mapgl::add_raster_layer(
      id                = id,
      source            = source_id,
      raster_opacity    = raster_opacity,
      raster_resampling = raster_resampling,
      before_id         = before_id, ...)
}


#' Build an msens cell tile URL template
#'
#' Returns an XYZ tile URL template (with `{z}/{x}/{y}` placeholders) for the
#' msens TiTiler factory. The SQL is canonicalized (whitespace collapsed) then
#' base64url-encoded. Consistent canonicalization is critical so repeated calls
#' with equivalent SQL produce identical URLs — Varnish keys on the full URL.
#'
#' @param sql character(1); SELECT returning `cell_id` and `value` columns
#' @param colormap character; rio-tiler colormap name (default: "spectral_r")
#' @param rescale numeric length-2 `c(min, max)` for normalization; `NULL` lets
#'   the server auto-compute from the SQL result on each tile request (prefer
#'   a client-side value from [cell_stats()] for cache stability)
#' @param v character; optional cache-bust tag (e.g. DB build date)
#' @param base character; base URL of the titilecache service
#' @return character(1) tile URL template
#' @export
#' @concept viz
cell_tile_url <- function(
    sql,
    colormap = "spectral_r", rescale = NULL, v = NULL,
    base = "https://titilecache.marinesensitivity.org") {
  stopifnot(is.character(sql), length(sql) == 1, nchar(sql) > 0)
  sql_b64 <- base64url_encode(canonicalize_sql(sql))
  params  <- c(sql = sql_b64, colormap = colormap)
  if (!is.null(rescale)) {
    stopifnot(is.numeric(rescale), length(rescale) == 2)
    params <- c(params, rescale = paste(rescale, collapse = ","))
  }
  if (!is.null(v))
    params <- c(params, v = as.character(v))
  qs <- paste0(names(params), "=", unname(params), collapse = "&")
  sprintf("%s/msens/tiles/{z}/{x}/{y}.png?%s", sub("/$", "", base), qs)
}


#' Fetch msens cell value statistics for a SQL query
#'
#' Calls the msens TiTiler factory `/statistics` endpoint. Returns a named list
#' with `n`, `min`, `max`, `mean`, `std`, `p2`, `p50`, `p98`. Use to set a
#' stable legend rescale that doesn't depend on per-tile computation.
#'
#' @param sql character(1); same SELECT passed to [cell_tile_url()]
#' @param base character; base URL of the titilecache service
#' @return named list of numeric statistics
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @export
#' @concept viz
cell_stats <- function(
    sql,
    base = "https://titilecache.marinesensitivity.org") {
  stopifnot(is.character(sql), length(sql) == 1, nchar(sql) > 0)
  sql_b64 <- base64url_encode(canonicalize_sql(sql))
  httr2::request(sprintf("%s/msens/statistics", sub("/$", "", base))) |>
    httr2::req_url_query(sql = sql_b64) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}


# internal helpers ----

#' @noRd
canonicalize_sql <- function(sql) {
  sql <- trimws(sql)
  gsub("[[:space:]]+", " ", sql)
}

#' @noRd
#' @importFrom base64enc base64encode
base64url_encode <- function(x) {
  b64 <- base64enc::base64encode(charToRaw(x))
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  sub("=+$", "", b64)
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

#' Build a flower-plot ggplot for component scores
#'
#' Polar bar chart of component scores with a center-label weighted
#' score. Returns a plain ggplot object (no interactive wrapping), so
#' it is usable in static outputs (pdf/docx). For an interactive
#' version, use [plot_flower()] with `interactive = TRUE` (the
#' default).
#'
#' @param data a tibble with one row per component
#' @param fld_category bare column name for the category (fill)
#' @param fld_height bare column name for the bar height (score)
#' @param fld_width bare column name for the bar width
#' @param tooltip_expr optional glue string for tooltip text
#' @param score optional pre-computed weighted-mean score for the center
#' @param title optional plot title
#' @return a ggplot
#' @importFrom ggplot2 ggplot aes scale_fill_manual coord_polar xlim
#'   annotate theme_minimal theme unit ggtitle
#' @importFrom ggiraph geom_rect_interactive
#' @importFrom dplyr arrange mutate summarize pull across lag lead
#'   where
#' @importFrom rlang ensym `:=`
#' @importFrom scales hue_pal
#' @importFrom glue glue
#' @export
#' @concept viz
ggplot_flower <- function(
  data,
  fld_category,
  fld_height,
  fld_width,
  tooltip_expr = NULL,
  score = NULL,
  title = NULL
) {
  stopifnot(is.numeric(data |> dplyr::pull({{ fld_height }})))
  stopifnot(is.numeric(data |> dplyr::pull({{ fld_width }})))

  if (is.null(score)) {
    score <- data |>
      dplyr::mutate(
        "{{fld_height}}" := as.double({{ fld_height }}),
        "{{fld_width}}"  := as.double({{ fld_width }})) |>
      dplyr::summarize(
        score = stats::weighted.mean(
          {{ fld_height }}, {{ fld_width }}, na.rm = TRUE)) |>
      dplyr::pull(score)
  }

  d <- data |>
    dplyr::arrange({{ fld_category }}) |>
    dplyr::mutate(dplyr::across(!dplyr::where(is.character), as.double)) |>
    dplyr::mutate(
      ymax = cumsum({{ fld_width }}),
      ymin = dplyr::lag(ymax, default = 0),
      xmax = {{ fld_height }},
      xmin = 0)

  if (!is.null(tooltip_expr)) {
    d <- d |> dplyr::mutate(tooltip = glue::glue(tooltip_expr))
  } else {
    d <- d |> dplyr::mutate(tooltip = as.character({{ fld_category }}))
  }

  components <- c(
    "invertebrate", "mammal", "other", "primprod",
    "turtle", "bird", "coral", "fish")
  cols <- stats::setNames(
    scales::hue_pal()(length(components)),
    components)

  g <- ggplot2::ggplot(d) +
    ggiraph::geom_rect_interactive(
      ggplot2::aes(
        xmin    = xmin,
        xmax    = xmax,
        ymin    = ymin,
        ymax    = ymax,
        fill    = {{ fld_category }},
        color   = "white",
        data_id = {{ fld_category }},
        tooltip = tooltip),
      color = "white",
      alpha = 0.5) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(-10, max(data |> dplyr::pull({{ fld_height }})))) +
    ggplot2::annotate(
      "text",
      x        = -10,
      y        = 0,
      label    = round(score),
      size     = 8,
      fontface = "bold") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin     = ggplot2::unit(c(20, 20, 20, 20), "pt"))

  if (!is.null(title)) g <- g + ggplot2::ggtitle(title)
  g
}

#' Flower plot of component scores (interactive or static)
#'
#' Builds a [`ggplot_flower()`] and optionally wraps it as an
#' interactive [`ggiraph::girafe`] for HTML output. Pass
#' `interactive = FALSE` for pdf/docx.
#'
#' @inheritParams ggplot_flower
#' @param interactive logical; if TRUE (default) return a girafe
#'   htmlwidget, otherwise return the plain ggplot
#' @return a girafe htmlwidget or a ggplot
#' @importFrom ggiraph girafe opts_sizing opts_tooltip
#' @export
#' @concept viz
plot_flower <- function(
  data,
  fld_category,
  fld_height,
  fld_width,
  tooltip_expr = NULL,
  score = NULL,
  title = NULL,
  interactive = TRUE
) {
  g <- ggplot_flower(
    data, {{ fld_category }}, {{ fld_height }}, {{ fld_width }},
    tooltip_expr = tooltip_expr, score = score, title = title)
  if (!interactive) return(g)
  ggiraph::girafe(
    ggobj = g,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_tooltip(
        css = "background-color:white;color:black;padding:5px;border-radius:3px;")))
}

#' Species table (interactive DT or static gt)
#'
#' Renders a species tibble — as returned by [species_for_cells()] —
#' as either an interactive [`DT::datatable`] (html output) or a
#' static [`gt::gt`] table (pdf/docx output). Taxon and model
#' columns become clickable links in the interactive version.
#'
#' @param d_spp tibble with columns from [species_for_cells()]:
#'   `mdl_seq`, `sp_cat`, `sp_common`, `sp_scientific`, `taxon_id`,
#'   `taxon_authority`, `er_code`, `er_score`, `is_mmpa`, `is_mbta`,
#'   `area_km2`, `avg_suit`, `pct_cat`
#' @param interactive logical; if TRUE (default) return a DT
#'   datatable, otherwise return a gt table
#' @return a DT htmlwidget or a gt_tbl
#' @importFrom dplyr mutate select rename arrange relocate if_else
#' @importFrom glue glue
#' @importFrom DT datatable formatPercentage formatSignif
#' @importFrom gt gt fmt_percent fmt_number cols_label
#' @export
#' @concept viz
tbl_species <- function(d_spp, interactive = TRUE) {
  d <- d_spp |>
    dplyr::mutate(
      model_url = glue::glue("../mapsp/?mdl_seq={mdl_seq}"),
      taxon_str = glue::glue("{taxon_authority}:{taxon_id}"),
      taxon_url = dplyr::if_else(
        taxon_authority == "botw",
        "https://birdsoftheworld.org",
        glue::glue(
          "https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}"))) |>
    dplyr::select(
      component     = sp_cat,
      taxon_authority,
      taxon_id,
      taxon_str,
      taxon_url,
      scientific    = sp_scientific,
      common        = sp_common,
      er_code,
      er_score,
      is_mmpa,
      is_mbta,
      model_id      = mdl_seq,
      model_url,
      area_km2,
      avg_suit,
      pct_component = pct_cat) |>
    dplyr::arrange(component, scientific)

  if (interactive) {
    d |>
      dplyr::mutate(
        taxon = glue::glue(
          '<a href="{taxon_url}" target="_blank">{taxon_str}</a>'),
        model = glue::glue(
          '<a href="{model_url}" target="_blank">{model_id}</a>')) |>
      dplyr::relocate(taxon, .after = component) |>
      dplyr::relocate(model, .after = er_score) |>
      dplyr::select(
        -taxon_id, -taxon_authority, -taxon_str, -taxon_url,
        -model_url, -model_id) |>
      dplyr::rename(cat = component, pct_cat = pct_component) |>
      DT::datatable(
        escape        = FALSE,
        rownames      = FALSE,
        fillContainer = TRUE,
        filter        = "top",
        class         = "display compact",
        extensions    = c("ColReorder", "KeyTable", "Responsive"),
        options = list(
          colReorder = TRUE,
          keys       = TRUE,
          pageLength = 5,
          lengthMenu = c(5, 50, 100),
          scrollX    = TRUE,
          dom        = "lfrtip")) |>
      DT::formatPercentage("er_score", 0) |>
      DT::formatPercentage(c("avg_suit", "pct_cat"), 2) |>
      DT::formatSignif("area_km2", 4)
  } else {
    d |>
      dplyr::select(
        cat      = component,
        scientific, common, er_code, er_score,
        area_km2, avg_suit,
        pct_cat  = pct_component) |>
      gt::gt() |>
      gt::fmt_percent(
        columns = c(er_score, avg_suit, pct_cat), decimals = 0) |>
      gt::fmt_number(columns = area_km2, decimals = 0) |>
      gt::cols_label(
        cat        = "Component",
        scientific = "Scientific name",
        common     = "Common name",
        er_code    = "Status",
        er_score   = "Ext. risk",
        area_km2   = "Area (km\u00b2)",
        avg_suit   = "Avg suit.",
        pct_cat    = "% cat.")
  }
}

#' Static ggplot map of labeled areas
#'
#' Returns a [`ggplot2::ggplot`] map of labeled sf polygons overlaid
#' on an [`rnaturalearth::ne_countries`] basemap, for embedding in
#' pdf/docx Quarto output where interactive [`mapgl`] maps do not
#' render.
#'
#' @param areas_sf an sf object with a `label` column
#' @param fill_color fill color for area polygons (default "#3388ff")
#' @param fill_alpha fill alpha (default 0.4)
#' @return a ggplot
#' @importFrom ggplot2 ggplot geom_sf aes coord_sf theme_minimal
#'   labs element_blank theme
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_bbox st_as_sfc
#' @export
#' @concept viz
ggmap_areas <- function(areas_sf, fill_color = "#3388ff",
                        fill_alpha = 0.4) {
  world <- rnaturalearth::ne_countries(
    scale = "medium", returnclass = "sf")
  bb <- sf::st_bbox(areas_sf)
  dx <- (bb["xmax"] - bb["xmin"]) * 0.1
  dy <- (bb["ymax"] - bb["ymin"]) * 0.1
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, fill = "#e8e4dc", color = "#b9b3a6",
                     linewidth = 0.2) +
    ggplot2::geom_sf(data = areas_sf, fill = fill_color,
                     alpha = fill_alpha, color = "#1e5a9e",
                     linewidth = 0.4) +
    ggplot2::coord_sf(
      xlim = c(bb["xmin"] - dx, bb["xmax"] + dx),
      ylim = c(bb["ymin"] - dy, bb["ymax"] + dy),
      expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(x = NULL, y = NULL)
}

utils::globalVariables(c(
  "xmin", "xmax", "ymin", "ymax", "tooltip",
  "sp_cat", "sp_common", "sp_scientific", "taxon_id", "taxon_authority",
  "mdl_seq", "taxon_str", "taxon_url", "model_url", "model_id",
  "component", "scientific", "common", "er_code", "er_score",
  "is_mmpa", "is_mbta", "area_km2", "avg_suit", "pct_cat",
  "pct_component", "taxon", "model", "cat"))
