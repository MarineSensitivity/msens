# Cells intersecting a polygon

Given an sf polygon and a cell-id raster, return a tibble of
intersecting `cell_id` with `pct_covered` (0-100). The cell raster uses
0-360 longitudes, so the input polygon is transformed and shifted
accordingly.

## Usage

``` r
cells_in_polygon(poly, r_cell_id)
```

## Arguments

- poly:

  an sf polygon (assumed or transformable to EPSG:4326)

- r_cell_id:

  a single-layer
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  of integer cell ids

## Value

a tibble with columns `cell_id` (integer) and `pct_covered` (0-100)
