# Weighted mean of component scores

Convenience wrapper returning the weighted mean of the `score` column
from
[`scores_for_cells()`](http://marinesensitivity.org/msens/reference/scores_for_cells.md),
weighted by `even`.

## Usage

``` r
mean_score(d_scores)
```

## Arguments

- d_scores:

  tibble from
  [`scores_for_cells()`](http://marinesensitivity.org/msens/reference/scores_for_cells.md)

## Value

a numeric scalar
