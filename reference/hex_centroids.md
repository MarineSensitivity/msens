# Centroids (lon/lat) for BIGINT H3 ids

Centroids (lon/lat) for BIGINT H3 ids

## Usage

``` r
hex_centroids(hex_id, con = NULL)
```

## Arguments

- hex_id:

  integer64/numeric vector of BIGINT H3 ids

- con:

  optional DuckDB connection (a temporary integer64 one if `NULL`)

## Value

tibble(hex_id, lon, lat)
