# Cells covered by each zone, on a given grid

The (zone × grid) intersection — which is a function of **geometry and
grid alone**, not of any release. `score_zones.qmd` recomputes it per
version, so the six releases sharing `programarea_2026-03` each paid for
the same `exactextractr` pass; keyed on `(zone_set_key, grid_id)` it is
computed once.

## Usage

``` r
zone_cells(ply, cellid_tif, key_col)
```

## Arguments

- ply:

  an `sf` of zone polygons

- cellid_tif:

  path to that grid's cell-id COG

- key_col:

  column of `ply` holding the zone key

## Value

a data frame with `zone_key`, `cell_id`, `pct_covered` (1-100)

## Details

Reads the grid's **cell-id COG**, whose pixel VALUES are cell ids (a
lookup image — see `grid.R`), so the returned ids are in that grid's
id-space whatever frame the raster itself uses.

Semantics deliberately match the per-version implementation this
replaces, so relocating it cannot move any score: coverage is rounded to
whole percent and anything rounding to zero is dropped.
