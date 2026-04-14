# Cell-ID SpatRaster

Return the global cell-id
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
used to identify cells for
[`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md)
and related helpers. The raster is shared across versions (not
version-specific).

## Usage

``` r
cell_id_raster()
```

## Value

a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with a `cell_id` layer
