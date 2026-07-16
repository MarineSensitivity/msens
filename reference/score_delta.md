# Join two versions' scores and compute per-key deltas (pure)

Inner-joins two score tables on `key` and returns the paired values plus
`delta = value_b - value_a`. Pure (data-frame in, data-frame out) so it
can be unit-tested without a database.

## Usage

``` r
score_delta(
  df_a,
  df_b,
  key = "programarea_key",
  value = "score",
  labels = c("a", "b")
)
```

## Arguments

- df_a, df_b:

  data frames each with columns `key` and `value`

- key:

  join column name (default `"programarea_key"`)

- value:

  value column name (default `"score"`)

- labels:

  length-2 suffixes for the two versions (default `c("a","b")`)

## Value

a tibble with `key`, `<value>_<labels[1]>`, `<value>_<labels[2]>`,
`delta`, sorted by descending `abs(delta)`
