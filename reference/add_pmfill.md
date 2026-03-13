# Add a PMTile polygon fill layer to a map

Adds a PMTiles source and fill layer with match_expr coloring. Works
with both initial map widgets and mapboxgl_proxy() updates.

## Usage

``` r
add_pmfill(
  m,
  url,
  source_layer,
  col_key,
  d = NULL,
  col_value = NULL,
  colors = NULL,
  filter_keys = NULL,
  id = "main_fill",
  source_id = "main_src",
  n_colors = 11,
  palette = "Spectral",
  reverse_palette = TRUE,
  fill_opacity = 0.7,
  outline_color = NULL,
  outline_width = 1,
  tooltip = NULL,
  popup = NULL,
  hover_options = NULL,
  before_id = NULL
)
```

## Arguments

- m:

  map or map_proxy

- url:

  PMTiles URL

- source_layer:

  source layer name

- col_key:

  key column name in PMTiles features

- d:

  data frame with col_key and col_value columns (for match_expr)

- col_value:

  score column for continuous coloring (or NULL)

- colors:

  named character vector of key-\>color (for categorical; or NULL)

- filter_keys:

  character vector of keys to include

- id:

  fill layer id (default: "main_fill")

- source_id:

  PMTiles source id (default: "main_src")

- n_colors:

  integer; number of color steps (default: 11)

- palette:

  character; RColorBrewer palette name (default: "Spectral")

- reverse_palette:

  logical; reverse palette (default: TRUE)

- fill_opacity:

  numeric (default: 0.7)

- outline_color:

  outline color for a companion line layer (NULL to skip)

- outline_width:

  outline width (default: 1)

- tooltip:

  passed to add_fill_layer

- popup:

  passed to add_fill_layer

- hover_options:

  passed to add_fill_layer

- before_id:

  layer to insert before

## Value

map with legend_meta attribute (list with rng, colors, categorical)
