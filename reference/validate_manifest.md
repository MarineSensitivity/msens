# Validate a version manifest

Split from
[`atlas_manifest()`](http://marinesensitivity.org/msens/reference/atlas_manifest.md)
so the publishing notebook asserts the same contract it writes, and the
unit tests can exercise it without network.

## Usage

``` r
validate_manifest(m, ver = NULL)
```

## Arguments

- m:

  parsed manifest (list)

- ver:

  expected version, or `NULL` to skip the cross-check

## Value

`m`, invisibly-but-returned (with `$ver` guaranteed)
