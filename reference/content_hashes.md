# Content fingerprint of every model in a surface table

Content fingerprint of every model in a surface table

## Usage

``` r
content_hashes(con, from, by, cols = c("cell_id", "val"))
```

## Arguments

- con:

  open DuckDB connection

- from:

  table name or `read_parquet(...)` expression

- by:

  grouping column — the per-release model id

- cols:

  payload columns (default `c("cell_id","val")`; v1-v7 use `"value"`)

## Value

a data frame with `by`, `n` and a 16-char `content_hash`
