# Order-invariant geometry fingerprint of a zone layer

Hashes each feature's WKB, ordered by the zone key (not by row order),
so a layer re-saved with features shuffled fingerprints identically
while any real change to a boundary does not.

## Usage

``` r
zone_geom_hash(x, key_col = NULL, layer = NULL)
```

## Arguments

- x:

  an `sf` object, or a path to a vector file (e.g. `.gpkg`)

- key_col:

  column holding the zone key; by default the first column whose name
  ends in `key`, else row order

- layer:

  layer name when `x` is a multi-layer file (default: the first)

## Value

a list with `n`, `key_col`, `keys` and a 16-char `geom_hash`
