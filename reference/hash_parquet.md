# Order-independent content fingerprint of Parquet file(s)

Scans the ON-DISK Parquet (cheap — never re-runs the ingest) and reduces
it to a short fingerprint invariant to row order, row-group boundaries,
compression and Parquet metadata timestamps. Works for surface files
(`mdl_key,cell_id,val`) and registry tables alike; columns are
introspected unless given.

## Usage

``` r
hash_parquet(glob, con = NULL, cols = NULL)
```

## Arguments

- glob:

  a `read_parquet` glob/path (e.g. `"dir/*.parquet"`,
  `"dir/**/*.parquet"`)

- con:

  optional DuckDB connection; a temp in-memory one is used if `NULL`

- cols:

  optional character vector of columns to hash (default: all)

## Value

a 16-char hex fingerprint string
