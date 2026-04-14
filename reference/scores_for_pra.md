# Precomputed component scores for a Program Area

Reads the precomputed Program Area metrics from the `zone_metric` table
instead of aggregating across cells. Returns the same shape as
[`scores_for_cells()`](http://marinesensitivity.org/msens/reference/scores_for_cells.md)
so it's a drop-in replacement for the score / flower-plot pipeline when
the area is a Program Area.

## Usage

``` r
scores_for_pra(con, pra_key, metric_pattern = "_ecoregion_rescaled$")
```

## Arguments

- con:

  a DBI connection (e.g. from
  [`sdm_db_con()`](http://marinesensitivity.org/msens/reference/sdm_db_con.md))

- pra_key:

  Program Area key (e.g. "CGM")

- metric_pattern:

  regex to filter `metric.metric_key` (default:
  `"_ecoregion_rescaled$"`)

## Value

tibble(metric_key, score, component, even)
