# Order-independent content fingerprint of a DuckDB table/query

Same reduction as
[`hash_parquet()`](http://marinesensitivity.org/msens/reference/hash_parquet.md)
but over a DuckDB table or query — for targets whose real output is a DB
table (e.g. `merge_taxon`, `score_*`) rather than a file.

## Usage

``` r
hash_query(con, sql, cols = NULL)
```

## Arguments

- con:

  open DuckDB connection

- sql:

  a table name OR a SELECT (no trailing `;`)

- cols:

  optional character vector of columns to hash (default: all)

## Value

a 16-char hex fingerprint string
