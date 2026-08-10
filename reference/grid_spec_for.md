# Grid spec by id, for [`publish_cog()`](http://marinesensitivity.org/msens/reference/publish_cog.md)

Same plain-list shape
[`grid_spec()`](http://marinesensitivity.org/msens/reference/grid_spec.md)
returns from a raster, plus `grid_id` and `lon360`. When the grid's
cell-id COG is on hand its geometry is read from the file
(authoritative, and keeps already-published v8 COGs bit-comparable,
since the real raster carries ~6e-6 deg of float drift from the nominal
values); the registry values are the offline fallback and are asserted
against the raster.

## Usage

``` r
grid_spec_for(grid_id, cellid_tif = NULL)
```

## Arguments

- grid_id:

  one of
  [`grid_registry()`](http://marinesensitivity.org/msens/reference/grid_registry.md)`$grid_id`

- cellid_tif:

  optional path to that grid's cell-id COG

## Value

a list with `nc`, `nr`, `xmin`, `ymax`, `resx`, `resy`, `crs`,
`grid_id`, `lon360`
