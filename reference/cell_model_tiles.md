# Spatial tile ids covering a set of cells

R-side twin of
[`cell_model_tile_sql()`](http://marinesensitivity.org/msens/reference/cell_model_tile_sql.md).
Use it to add a `tile IN (…)` filter so DuckDB prunes to the relevant
partitions — **without it a query reads every partition**, which is the
whole problem `cell_model` exists to solve.

## Usage

``` r
cell_model_tiles(cell_id)
```

## Arguments

- cell_id:

  integer vector of cell ids

## Value

sorted unique integer tile ids

## Examples

``` r
cell_model_tiles(c(1080221L, 1080222L))
#> [1] 436
```
