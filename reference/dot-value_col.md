# Name of a table's scalar value column (`val` or `value`)

The v8 schema renamed the score/key column from `value` to `val`
(`value` is a DuckDB reserved word). Detecting it per-connection lets
[`pra_score_delta()`](http://marinesensitivity.org/msens/reference/pra_score_delta.md)
compare a v7 database (`value`) against a v8 database (`val`) with one
query shape. Prefers `val` when both somehow exist.

## Usage

``` r
.value_col(con, tbl)
```

## Arguments

- con:

  a DBI connection

- tbl:

  table name to inspect

## Value

`"val"` or `"value"`
