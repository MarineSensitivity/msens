# Enumerate the ocean H3 hex tiling from a raster mask (GEOMETRY ONLY)

Defines *which* res-`res` hexagons tile the ocean and their `area_km2` —
it does **not** interpolate any values (see the module INTERPOLATION
PRINCIPLE). A res-7 hex (~5.16 km^2) is smaller than a 0.05 degree pixel
(~23 km^2), so each ocean pixel (non-NA in `mask_layer`) is polyfilled
into the hexes whose *centre* falls inside it
(`h3_polygon_wkt_to_cells`, center-containment → a gap-free,
overlap-free tiling). Values are attached afterwards with
[`hex_interp_idw()`](http://marinesensitivity.org/msens/reference/hex_interp_idw.md)
from the source centroids.

## Usage

``` r
hex_ocean(r, con, res = HEX_RES, mask_layer = names(r)[1], out_tbl = "hex")
```

## Arguments

- r:

  a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  in EPSG:4326

- con:

  a DuckDB connection

- res:

  H3 resolution (default
  [HEX_RES](http://marinesensitivity.org/msens/reference/HEX_RES.md))

- mask_layer:

  layer whose non-NA pixels define ocean coverage (default
  `names(r)[1]`)

- out_tbl:

  DuckDB table to (over)write with columns `hex_id` (BIGINT) and
  `area_km2` (default `"hex"`)

## Value

invisibly, `out_tbl`
