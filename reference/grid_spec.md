# Grid spec for the global cell-id raster

Plain-list description of the global 0.05° -180,180 cell-id grid, so
[`publish_cog()`](http://marinesensitivity.org/msens/reference/publish_cog.md)
can construct a windowed raster from a model's `cell_id`s without
serializing a terra SpatRaster across parallel workers. `cell_id` is the
row-major (top-left origin) 1-based cell index of this grid, so a cell's
row/col is pure arithmetic (no lookup against the COG).

## Usage

``` r
grid_spec(r)
```

## Arguments

- r:

  a terra SpatRaster of the cell-id COG (`r_cellid_global.tif`)

## Value

a list with `nc`, `nr`, `xmin`, `ymax`, `resx`, `resy`, `crs`
