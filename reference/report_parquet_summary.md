# Summarize a set of atlas Parquet files for a notebook's `## Outputs` section

One-row summary of a `read_parquet` glob: `n_rows`, `n_models` (distinct
`mdl_key`, when present), `val_min`/`val_max` (when a `val` column is
present), `n_files` and `size_mb`. Pair with
[`report_table()`](http://marinesensitivity.org/msens/reference/report_table.md)
and the `## Outputs` section convention.

## Usage

``` r
report_parquet_summary(glob, con = NULL)
```

## Arguments

- glob:

  a `read_parquet` glob/path

- con:

  optional DuckDB connection; a temp in-memory one is used if `NULL`

## Value

a one-row tibble
