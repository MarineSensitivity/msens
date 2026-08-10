# Index the existing COG store

ONE recursive listing into a set, so "is this already published?" is a
lookup rather than a HEAD request per model — at ~30k models per release
the per-object form is the difference between seconds and hours.

## Usage

``` r
cog_store_index(
  grid_id = NULL,
  bucket = "s3://oceanmetrics.io-public/marine-atlas",
  aws = "aws"
)
```

## Arguments

- grid_id:

  restrict to one grid's prefix, or `NULL` for the whole store

- bucket:

  S3 URI of the atlas root

- aws:

  path to the `aws` CLI

## Value

character vector of `content_hash` values already present
