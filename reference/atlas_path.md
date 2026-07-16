# Path to a release component under the atlas base

Path to a release component under the atlas base

## Usage

``` r
atlas_path(con, ...)
```

## Arguments

- con:

  connection from
  [`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)

- ...:

  path parts under the version root, e.g. `"tables"`, `"taxon.parquet"`

## Value

an `s3://…` string
