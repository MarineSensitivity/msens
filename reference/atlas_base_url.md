# Base HTTPS URL of the marine-atlas release tree

Path-style, because the bucket name contains dots and so breaks
virtual-hosted-style TLS (see
[`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)
for the same constraint).

## Usage

``` r
atlas_base_url(
  bucket = "s3://oceanmetrics.io-public/marine-atlas",
  region = "us-east-1"
)
```

## Arguments

- bucket:

  S3 URI of the atlas root

- region:

  S3 region

## Value

an `https://` base URL with no trailing slash
