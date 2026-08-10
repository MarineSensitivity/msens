# The atlas version registry

Parses `versions.json`. `status` is one of `released`, `prerelease`,
`retired`; anything else is rejected, because an unrecognized status
silently becoming "showable" is how a half-built release reaches users.

## Usage

``` r
atlas_versions(base = atlas_base_url(), refresh = FALSE)
```

## Arguments

- base:

  atlas base URL from
  [`atlas_base_url()`](http://marinesensitivity.org/msens/reference/atlas_base_url.md)

- refresh:

  re-fetch instead of using the session cache

## Value

a data frame with at least `ver` and `status`, newest first
