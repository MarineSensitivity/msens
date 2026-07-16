# Configure a DuckDB connection to read the marine-atlas release from S3

Loads `httpfs` (+ `aws` for credentialed listing), sets path-style
addressing + region, and (unless `anon`) creates a credential-chain S3
secret. Returns the connection with the atlas base URL stored in
`attr(con, "atlas_base")`.

## Usage

``` r
attach_atlas(
  con = NULL,
  version = "v8",
  anon = FALSE,
  views = TRUE,
  bucket = "s3://oceanmetrics.io-public/marine-atlas",
  region = "us-east-1"
)
```

## Arguments

- con:

  an open DuckDB connection, or `NULL` to open a fresh in-memory one

- version:

  atlas version (e.g. `"v8"`)

- anon:

  if `TRUE`, skip credentials (single-file public reads only; globs need
  LIST and will 403 unless the bucket policy allows anonymous
  ListBucket)

- views:

  if `TRUE` (default), also create named views over the released tables
  (via
  [`atlas_views()`](http://marinesensitivity.org/msens/reference/atlas_views.md))
  so the calc/score helpers compose directly — e.g.
  `scores_for_pra(attach_atlas(), pra_key)` just works

- bucket:

  S3 base of the atlas

- region:

  S3 region

## Value

the (configured) DuckDB connection, with the atlas base URL in
`attr(con, "atlas_base")` and (unless `views = FALSE`) the atlas table
views
