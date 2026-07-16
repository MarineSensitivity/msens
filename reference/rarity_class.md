# Rarity class from range size (pure)

Bins a global range size (km^2) into an ordered rarity class. Defaults
are a starting point to tune against the v8 range-size distribution.

## Usage

``` r
rarity_class(
  range_km2,
  breaks = c(10000, 1e+05, 1e+06),
  labels = c("very_rare", "rare", "common", "widespread")
)
```

## Arguments

- range_km2:

  numeric vector of range sizes (km^2)

- breaks:

  upper bounds (km^2) between classes (default `c(1e4, 1e5, 1e6)`)

- labels:

  ordered class labels (length `length(breaks) + 1`)

## Value

an ordered factor of rarity classes
