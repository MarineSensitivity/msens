# Program-Area composite-score delta between two version databases

Reads the Program-Area composite score from two SDM DuckDB connections
(e.g. v7 on the cell grid and v8 on the hex grid) and returns the
per-Program-Area
[`score_delta()`](http://marinesensitivity.org/msens/reference/score_delta.md).
Centralizes the query previously inlined in `workflows/dev/build_v7.R`
so `build_v8.R` and `validate_versions.qmd` share it.

## Usage

``` r
pra_score_delta(
  con_a,
  con_b,
  metric_key = METRIC_SCORE_DEFAULT,
  labels = c("v7", "v8"),
  zone_set_key = NULL
)
```

## Arguments

- con_a, con_b:

  DBI connections to the two versions' `sdm.duckdb`

- metric_key:

  composite metric key (default METRIC_SCORE_DEFAULT)

- labels:

  length-2 version labels (default `c("v7","v8")`)

- zone_set_key:

  optional `{zone_type}_{YYYY-MM}` to pin the spatial unit; errors if a
  connection carries the column but not that value

## Value

a tibble from
[`score_delta()`](http://marinesensitivity.org/msens/reference/score_delta.md)
keyed by `programarea_key`

## Details

Schema-adaptive: the score/key column is `value` in v7 and `val` in v8
(the reserved-word rename), so the column name is resolved per
connection via
[`.value_col()`](http://marinesensitivity.org/msens/reference/dot-value_col.md)
rather than hard-coded — otherwise a v7↔v8 (or v8↔v8) comparison errors
with "Table z does not have a column named value".

Pass `zone_set_key` to pin the comparison to ONE spatial unit. Without
it the query selects `fld = 'programarea_key'` from each database and
trusts that the two releases meant the same polygons. Measured across
every published gpkg, they do — every program-area layer from v2 through
v8 is one geometry — but that is a fact about the data, not a guarantee,
and BOEM's planning units keep changing. Naming the zone set makes the
assumption explicit and checkable; databases predating the column
(v1–v7) fall back to the `fld` filter.
