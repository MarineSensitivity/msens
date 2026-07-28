# Cells intersecting a polygon

Returns `cell_id` + `pct_covered` (0-100) for the cells a polygon
overlaps. `pct_covered` is the fraction of each cell inside the polygon,
and it is not cosmetic —
[`scores_for_cells()`](http://marinesensitivity.org/msens/reference/scores_for_cells.md)
and
[`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md)
weight by it, so partially covered edge cells count proportionally.

## Usage

``` r
cells_in_polygon(poly, src, res = 0.05)
```

## Arguments

- poly:

  an sf polygon (assumed or transformable to EPSG:4326)

- src:

  a DBI connection (**preferred**), or a single-layer
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  of integer cell ids

- res:

  grid resolution in degrees, SQL path only (default `0.05`)

## Value

a tibble with columns `cell_id` (integer) and `pct_covered` (0-100)

## Details

**Pass a DB connection.** Then the grid is read from the database being
queried and cannot disagree with it. Two paths, chosen automatically:

- **v8** (`cell` carries `lon`/`lat`) — a SQL bbox select on `cell`
  picks the candidates and `sf` computes exact coverage on just those.
  No raster is touched. For a 2x1.5-degree area this is **~0.02 s**
  against ~35 s to read the whole cell-id raster, and it joins
  `cell_model`, which is already cell-oriented.

- **v7** (no `lon`/`lat`) — falls back to
  [`cell_id_raster()`](http://marinesensitivity.org/msens/reference/cell_id_raster.md),
  the 0-360 regional raster that IS v7's grid.

Passing a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
directly still works, but nothing can then verify it matches the
database — see
[`cell_id_raster()`](http://marinesensitivity.org/msens/reference/cell_id_raster.md)
for how that failed silently on v8.
