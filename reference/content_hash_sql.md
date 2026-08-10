# Grouped content-fingerprint SQL

One pass, no sort and no spill — the whole point, since these tables run
to 1.2 billion rows (measured: 325M rows in 7.8 s, so a full release
hashes in under a minute).

## Usage

``` r
content_hash_sql(from, by, cols = c("cell_id", "val"))
```

## Arguments

- from:

  table name or `read_parquet(...)` expression

- by:

  grouping column — the per-release model id (`mdl_seq` or `mdl_id`)

- cols:

  payload columns to hash, in a FIXED order

## Value

a SQL string yielding `by`, `n`, `x`, `s`
