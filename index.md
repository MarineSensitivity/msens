# msens

R library of functions for mapping marine sensitivities, sponsored by
BOEM

## Install

Install this R package by running the following command in your R
Console:

``` r

remotes::install_github("MarineSensitivity/msens")
```

## Testing

`msens` is where the pipeline’s scientific logic lives (merge, scoring,
ingest, interpolation), so it is where that logic is **guarded by unit
tests**. Run them with:

``` r

devtools::test()                                   # all tests
testthat::test_file("tests/testthat/test-merge.R") # one file
```

Conventions:

- **Keep model rules in exported functions**
  (e.g. [`merge_sql()`](http://marinesensitivity.org/msens/reference/merge_sql.md),
  [`turtle_sql()`](http://marinesensitivity.org/msens/reference/turtle_sql.md),
  [`compute_er_score()`](http://marinesensitivity.org/msens/reference/compute_er_score.md))
  so the notebooks in `../workflows` *call* them and the tests *assert*
  them — they cannot drift.
- **Every rule has a testthat fixture** asserting its exact expected
  output. `test-merge.R` covers one synthetic taxon per merge category
  (range-only, both-masked, `iucn_range_outside_us_eez`-excluded,
  am-only single, am-only multi-model no-dedup, turtle multiplicative).
- **Add/update the test in the same change as the logic**, and keep a
  permanent regression assertion for every bug fixed, so it can never
  silently return. After edits, `devtools::document()` + reinstall so
  `../workflows` picks up the change.
