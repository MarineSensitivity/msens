# Aggregate component scores across a set of cells

Weighted-mean aggregation of `cell_metric` across a set of cells;
returns a flower-plot-ready tibble with columns `metric_key`, `score`,
`component`, `even`.

## Usage

``` r
scores_for_cells(con, cells, metric_pattern = "_ecoregion_rescaled$")
```

## Arguments

- con:

  a DBI connection (e.g. from
  [`sdm_db_con()`](http://marinesensitivity.org/msens/reference/sdm_db_con.md))

- cells:

  a tibble from
  [`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md),
  with columns `cell_id` and `pct_covered`

- metric_pattern:

  regex to filter `metric.metric_key` (default:
  `"_ecoregion_rescaled$"`)

## Value

tibble(metric_key, score, component, even)
