# Program-Area composite-score delta between two version databases

Reads the Program-Area composite score from two SDM DuckDB connections
(e.g. v7 on the cell grid and v8 on the hex grid) and returns the
per-Program-Area
[`score_delta()`](http://marinesensitivity.org/msens/reference/score_delta.md).
Centralizes the query previously inlined in `workflows/dev/build_v7.R`
so `build_v8.R` and `validate_v7_v8.qmd` share it.

## Usage

``` r
pra_score_delta(
  con_a,
  con_b,
  metric_key = METRIC_SCORE_DEFAULT,
  labels = c("v7", "v8")
)
```

## Arguments

- con_a, con_b:

  DBI connections to the two versions' `sdm.duckdb`

- metric_key:

  composite metric key (default METRIC_SCORE_DEFAULT)

- labels:

  length-2 version labels (default `c("v7","v8")`)

## Value

a tibble from
[`score_delta()`](http://marinesensitivity.org/msens/reference/score_delta.md)
keyed by `programarea_key`
