# SQL expression for a cell's spatial tile id

The single definition of the `cell_model` partition key, used by the
release notebook when writing and by
[`cell_model_tiles()`](http://marinesensitivity.org/msens/reference/cell_model_tiles.md)
when reading.

## Usage

``` r
cell_model_tile_sql(col = "cell_id", ncol = .CELL_GRID_NCOL)
```

## Arguments

- col:

  SQL expression giving the cell id, e.g. `"c.cell_id"`

- ncol:

  grid width in columns (default v8's 7200; v7 is 3103 — see
  [`cell_grid_ncol()`](http://marinesensitivity.org/msens/reference/cell_grid_ncol.md))

## Value

character SQL expression

## Details

Note the integer division operator `//`: DuckDB's `/` is FLOAT division,
which silently yields a distinct "tile" per cell (and therefore one
partition per cell) if used here by mistake.

## Examples

``` r
cell_model_tile_sql("cell_id")
#> (((cell_id-1)//7200)//50)*144 + (((cell_id-1)%7200)//50)
cell_model_tile_sql("cell_id", ncol = 3103)   # v7 grid
#> (((cell_id-1)//3103)//50)*62 + (((cell_id-1)%3103)//50)
```
