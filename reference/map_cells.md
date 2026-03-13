# Map raster cells with outlines and labels

Convenience constructor that composes add_cells(), add_pmline(), and
add_pmlabel() into a complete map with legend and scale control.

## Usage

``` r
map_cells(
  r,
  colors = NULL,
  base_map = NULL,
  bounds = NULL,
  raster_opacity = 0.9,
  raster_resampling = "nearest",
  n_colors = 11,
  palette = "Spectral",
  reverse_palette = TRUE,
  legend_title = "Score",
  legend_position = "bottom-left",
  legend_values = NULL,
  pmtiles_outlines = NULL,
  labels = NULL
)
```

## Arguments

- r:

  terra SpatRaster

- colors:

  character vector of colors (or NULL to auto-generate)

- base_map:

  a mapboxgl or maplibre map object (or NULL for default dark)

- bounds:

  sf or bbox object to fit map bounds to

- raster_opacity:

  numeric (default: 0.9)

- raster_resampling:

  character (default: "nearest")

- n_colors:

  integer; number of color steps (default: 11)

- palette:

  character; RColorBrewer palette name (default: "Spectral")

- reverse_palette:

  logical; reverse palette (default: TRUE)

- legend_title:

  character (default: "Score")

- legend_position:

  character (default: "bottom-left")

- legend_values:

  numeric range for legend (or NULL for auto)

- pmtiles_outlines:

  list of outline specs for add_pmline()

- labels:

  list of label specs for add_pmlabel()

## Value

a mapboxgl htmlwidget
