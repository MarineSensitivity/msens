# Add an msens cell tile layer to a mapgl map

Viewport-driven XYZ tile alternative to
[`add_cells()`](http://marinesensitivity.org/msens/reference/add_cells.md):
the browser only fetches the tiles visible in the current viewport from
the msens TiTiler factory, rather than shipping the whole raster as a
base64-encoded image.

## Usage

``` r
add_cell_tiles(
  m,
  tile_url,
  id = "r_lyr",
  source_id = "r_src",
  tile_size = 256,
  raster_opacity = 0.8,
  raster_resampling = "nearest",
  before_id = NULL,
  ...
)
```

## Arguments

- m:

  map or map_proxy

- tile_url:

  character(1) XYZ tile URL template, typically from
  [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)
  (must contain `{z}/{x}/{y}` placeholders)

- id:

  layer id (default: "r_lyr")

- source_id:

  source id (default: "r_src")

- tile_size:

  numeric (default: 256)

- raster_opacity:

  numeric (default: 0.8)

- raster_resampling:

  character (default: "nearest")

- before_id:

  layer to insert before

- ...:

  additional args to
  [`mapgl::add_raster_layer()`](https://walker-data.com/mapgl/reference/add_raster_layer.html)

## Value

map (pipeable)
