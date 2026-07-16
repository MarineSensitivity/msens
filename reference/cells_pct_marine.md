# Percent of a model's cells that are marine (ocean)

A derived metric for a range that spans land + ocean: the share of its
cells that are ocean. v8 keeps the whole range
([`cells_from_ranges()`](http://marinesensitivity.org/msens/reference/cells_from_ranges.md));
this reports how marine it is (e.g. a seabird ~30% marine, a whale
~100%).

## Usage

``` r
cells_pct_marine(cell_ids, ocean_cell_ids, area_km2 = NULL)
```

## Arguments

- cell_ids:

  integer cell ids of a model's cells

- ocean_cell_ids:

  integer cell ids that are ocean (e.g. the `cell` table's)

- area_km2:

  optional per-cell areas aligned to `cell_ids` for an area-weighted
  percent; `NULL` (default) = cell-count percent

## Value

numeric percent in 0,100, or `NA` if `cell_ids` is empty
