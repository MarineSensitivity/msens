# Read a released derived table (`tables/<name>.parquet`) as a lazy relation

Read a released derived table (`tables/<name>.parquet`) as a lazy
relation

## Usage

``` r
atlas_tbl(con, name)
```

## Arguments

- con:

  connection from
  [`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)

- name:

  table name (`cell`, `taxon`, `dataset`, `model`, `cell_metric`,
  `zone`, `zone_cell`, `zone_metric`, `metric`)

## Value

a `dplyr` tbl over the remote Parquet
