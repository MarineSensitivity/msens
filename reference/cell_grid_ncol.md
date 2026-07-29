# Grid width (columns) a database's `cell_model` was tiled with

The tile key depends on how many columns the grid has, and that differs
by version (v8 global: 7200; v7 regional: 3103). Readers must use the
SAME width the writer used or `tile IN (…)` prunes away the very rows
being sought — and silently, since a wrong tile id is still a valid tile
id.

## Usage

``` r
cell_grid_ncol(con = NULL)
```

## Arguments

- con:

  a DBI connection, or `NULL` for the default

## Value

integer number of grid columns

## Details

Resolution order: the `cell_grid` table written alongside `cell_model`,
then the default. Every v8 database predates that table and correctly
falls back.
