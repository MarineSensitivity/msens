# Group zone layers into distinct vintages by geometry

Takes the fingerprints of many candidate layers and reports how many
genuinely distinct geometries they contain — the measurement that
decides how many zone sets must be scored, rather than assuming one per
release.

## Usage

``` r
zone_set_group(x, vintage_of = character())
```

## Arguments

- x:

  data frame with `source`, `zone_type` and `geom_hash`

- vintage_of:

  named character mapping `geom_hash` -\> `YYYY-MM`; hashes not named
  here are left `NA` for a human to label

## Value

`x` with `zone_set_key` and `vintage` columns
