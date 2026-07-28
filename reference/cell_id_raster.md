# Cell-ID SpatRaster (the **v7** grid)

`derived/r_bio-oracle_planarea.tif` — a **regional** raster on **0-360**
longitudes whose pixel values are **v7** `cell_id`s.

## Usage

``` r
cell_id_raster()
```

## Value

a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with a `cell_id` layer

## Details

It is NOT version-neutral, despite the generic name. v8 uses a different
grid entirely: global, `[-180,180]`, `cell_id` 1..24,293,128. Handing
this raster to
[`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md)
against a **v8** database therefore yields ids that do exist in v8 but
denote **completely different places** — a polygon off Santa Barbara
resolved to cells in the Arctic, so
[`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md)
returned zero species with no error at all. Prefer passing the DB
connection to
[`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md),
which picks the right grid for the version it is actually looking at.
