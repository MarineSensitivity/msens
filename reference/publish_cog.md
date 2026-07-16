# Publish one model's cells as a COG

Paints a model's `(cell_id, val)` onto the global 0.05° grid, cropped to
the cells' bounding box, and writes a Cloud-Optimized GeoTIFF with
internal overviews (so a global range renders cheaply at low zoom via
titiler `/cog`). Cropping to the data window keeps each COG small (most
ranges are regional).

## Usage

``` r
publish_cog(
  cell_id,
  val,
  out_tif,
  grid,
  datatype = "INT1U",
  nodata = 0,
  overview = TRUE
)
```

## Arguments

- cell_id:

  integer vector of global row-major cell ids (1-based)

- val:

  numeric vector of values aligned to `cell_id` (e.g. 1-100)

- out_tif:

  output path (`.tif`)

- grid:

  grid spec from
  [`grid_spec()`](http://marinesensitivity.org/msens/reference/grid_spec.md)

- datatype:

  terra datatype (default `"INT1U"`; use `"FLT4S"` for floats)

- nodata:

  value flagged as NoData (default `0`; AquaMaps vals are 1-100)

- overview:

  logical; build internal overviews (default `TRUE`)

## Value

`out_tif` (invisibly), or `NULL` if no finite cells
