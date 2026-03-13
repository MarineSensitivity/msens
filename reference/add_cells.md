# Add a raster cell layer to a map

Adds an image source from a terra SpatRaster and a raster layer. Works
with both initial map widgets and mapboxgl_proxy() updates.

## Usage

``` r
add_cells(
  m,
  r,
  colors,
  id = "r_lyr",
  source_id = "r_src",
  raster_opacity = 0.8,
  raster_resampling = "nearest",
  before_id = NULL,
  ...
)
```

## Arguments

- m:

  map or map_proxy

- r:

  terra SpatRaster

- colors:

  character vector of colors

- id:

  layer id (default: "r_lyr")

- source_id:

  source id (default: "r_src")

- raster_opacity:

  numeric (default: 0.8)

- raster_resampling:

  character (default: "nearest")

- before_id:

  layer to insert before

- ...:

  additional args to add_raster_layer

## Value

map (pipeable)
