# Validate a zone-set registry table

Enforces the two invariants that make cross-version comparison
meaningful: one key names exactly one geometry, and one geometry carries
exactly one key. Without the second, the same polygons published under
two keys would be scored twice and compared as though they were
different places.

## Usage

``` r
validate_zone_sets(d)
```

## Arguments

- d:

  data frame with `zone_set_key`, `zone_type`, `vintage`, `geom_hash`,
  `n_zones`

## Value

`d`, invisibly
