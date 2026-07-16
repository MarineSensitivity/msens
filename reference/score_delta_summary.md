# Summary statistics for a delta table (pure)

Summary statistics for a delta table (pure)

## Usage

``` r
score_delta_summary(d, delta_col = "delta")
```

## Arguments

- d:

  a tibble from
  [`score_delta()`](http://marinesensitivity.org/msens/reference/score_delta.md)

- delta_col:

  name of the delta column (default `"delta"`)

## Value

a named list: `n`, `mean_abs`, `max_abs`, `rmse`
