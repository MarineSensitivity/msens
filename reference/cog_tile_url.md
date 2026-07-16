# Build a titiler `/cog` tile URL template for a native COG

Returns an XYZ tile URL template (with `{z}/{x}/{y}`) for titiler's
standard COG tiler, used to render a native per-model COG (e.g. a raw
AquaMaps global range published by
[`publish_cog()`](http://marinesensitivity.org/msens/reference/publish_cog.md))
as a raster overlay via
[`add_cell_tiles()`](http://marinesensitivity.org/msens/reference/add_cell_tiles.md).
Unlike
[`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)
(dense per-cell SQL over a fixed cell-id COG, capped at ~1M cells), this
serves a pyramided COG so a whole global range renders cheaply at any
zoom.

## Usage

``` r
cog_tile_url(
  cog_url,
  colormap = "spectral_r",
  rescale = c(1, 100),
  base = "https://titiler-v8.marinesensitivity.org",
  tms = "WebMercatorQuad"
)
```

## Arguments

- cog_url:

  character(1); full HTTP(S) URL of the COG (e.g. a path-style S3 URL to
  `native/am/{key}.tif`)

- colormap:

  character; rio-tiler colormap name (default `"spectral_r"`)

- rescale:

  numeric length-2 `c(min, max)` (default `c(1, 100)`)

- base:

  character; base URL of the titiler service serving `/cog`

- tms:

  character; TileMatrixSet id in the route (default `"WebMercatorQuad"`)

## Value

character(1) tile URL template
