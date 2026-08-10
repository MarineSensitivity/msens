# List every object under a prefix

Uses the credentialed `aws` CLI rather than the anonymous XML API,
because this bucket denies anonymous `ListBucket`.

## Usage

``` r
s3_list_all(bucket = "s3://oceanmetrics.io-public", prefix = "", aws = "aws")
```

## Arguments

- bucket:

  S3 URI (e.g. `s3://oceanmetrics.io-public`)

- prefix:

  key prefix to list ("" for the whole bucket)

- aws:

  path to the `aws` CLI

## Value

a data frame of `key` and `size` (bytes)
