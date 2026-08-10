# The key column for a zone type

A zone gpkg typically carries SEVERAL `*_key` columns — the program-area
layer has `region_key`, `planarea_key` and `programarea_key` — so "the
first column ending in key" picks the wrong one. That is not a cosmetic
error: taking `region_key` (3 values) for the program-area layer yields
3 zones instead of 20, and, because the ordering it induces is not
total, leaves ties whose order depends on the file's feature order — so
two byte-identical layers saved in different order could fingerprint
differently.

## Usage

``` r
zone_key_col(zone_type, nms)
```

## Arguments

- zone_type:

  one of
  [`zone_set_key()`](http://marinesensitivity.org/msens/reference/zone_set_key.md)'s
  types

- nms:

  column names of the layer

## Value

the matching column name
