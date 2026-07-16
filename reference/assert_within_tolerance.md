# Assert a delta table is within tolerance, else error (pure)

The equivalence gate: fails (stops) if `mean|Δ|` exceeds `mean_tol` or
`max|Δ|` exceeds `max_tol`. Returns the summary invisibly on success so
a notebook can report it.

## Usage

``` r
assert_within_tolerance(d, mean_tol, max_tol, delta_col = "delta")
```

## Arguments

- d:

  a tibble from
  [`score_delta()`](http://marinesensitivity.org/msens/reference/score_delta.md)

- mean_tol:

  tolerance on mean absolute delta

- max_tol:

  tolerance on max absolute delta

- delta_col:

  name of the delta column (default `"delta"`)

## Value

(invisibly) the
[`score_delta_summary()`](http://marinesensitivity.org/msens/reference/score_delta_summary.md)
list
