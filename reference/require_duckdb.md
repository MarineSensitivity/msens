# Require a modern DuckDB (and optionally the spatial GEOMETRY extension)

Guards the v8 Parquet-V2 / byte-sized-row-group writers
([`copy_atlas_parquet()`](http://marinesensitivity.org/msens/reference/copy_atlas_parquet.md),
[`write_atlas_parquet()`](http://marinesensitivity.org/msens/reference/write_atlas_parquet.md))
and leaves room for a future GeoParquet cell-geometry column: checks the
installed `duckdb` R package is `>= min` and, when `spatial = TRUE`,
that `LOAD spatial` succeeds (native `GEOMETRY` type, DuckDB 1.5+).
Geometry is not yet persisted — `spatial` defaults `FALSE`.

## Usage

``` r
require_duckdb(min = "1.5.0", con = NULL, spatial = FALSE)
```

## Arguments

- min:

  minimum `duckdb` package version (default `"1.5.0"`)

- con:

  optional open connection to test `spatial` on (a temp in-memory one is
  used if `NULL`)

- spatial:

  also require the spatial extension (default `FALSE`)

## Value

`TRUE` invisibly, or stops
