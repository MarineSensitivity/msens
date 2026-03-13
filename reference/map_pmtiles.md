# Map data on PMTile vector tiles

Create a mapboxgl/maplibre map with polygon fill colors driven by a data
frame, using PMTile vector tile sources. Data is joined to features at
render time via match expressions, so PMTiles don't need to be
regenerated.

## Usage

``` r
map_pmtiles(
  d,
  col_key,
  col_value = NULL,
  colors = NULL,
  pmtiles_url,
  source_layer,
  base_map = NULL,
  bounds = NULL,
  filter_keys = NULL,
  n_colors = 11,
  palette = "Spectral",
  reverse_palette = TRUE,
  fill_opacity = 0.7,
  outline_color = "white",
  outline_width = 1,
  legend_title = "Score",
  legend_position = "bottom-left",
  categorical = FALSE,
  legend_labels = NULL,
  tooltip = NULL,
  popup = NULL,
  hover_options = NULL,
  pmtiles_outlines = NULL,
  labels = NULL
)
```

## Arguments

- d:

  data frame with a key column and either a score or color column

- col_key:

  character; name of the key column in `d`

- col_value:

  character; name of the score column for continuous coloring (or NULL)

- colors:

  character vector of colors per row (or named by key), for categorical
  coloring (or NULL)

- pmtiles_url:

  character; URL to the PMTiles file

- source_layer:

  character; source layer name within the PMTiles

- base_map:

  a mapboxgl or maplibre map object; if NULL creates mapboxgl(dark,
  globe)

- bounds:

  sf or bbox object to fit map bounds to

- filter_keys:

  character vector of keys to show (subset of d)

- n_colors:

  integer; number of color steps for continuous mode (default: 11)

- palette:

  character; RColorBrewer palette name (default: "Spectral")

- reverse_palette:

  logical; reverse palette so red=high (default: TRUE)

- fill_opacity:

  numeric (default: 0.7)

- outline_color:

  character (default: "white")

- outline_width:

  numeric (default: 1)

- legend_title:

  character (default: "Score")

- legend_position:

  character (default: "bottom-left")

- categorical:

  logical; use categorical legend (default: FALSE)

- legend_labels:

  character vector of labels for categorical legend

- tooltip:

  passed to add_fill_layer

- popup:

  passed to add_fill_layer

- hover_options:

  passed to add_fill_layer

- pmtiles_outlines:

  list of outline layer specs for add_pmline()

- labels:

  list of label specs for add_pmlabel()

## Value

a mapboxgl/maplibre htmlwidget (pipeable for additional layers)

## Details

Internally uses add_pmfill(), add_pmline(), and add_pmlabel().
