# The promoted (latest released) atlas version

Reads `latest.txt`. Errors if unreachable rather than guessing — see the
design note at the top of this file.

## Usage

``` r
atlas_latest(base = atlas_base_url(), refresh = FALSE)
```

## Arguments

- base:

  atlas base URL from
  [`atlas_base_url()`](http://marinesensitivity.org/msens/reference/atlas_base_url.md)

- refresh:

  re-fetch instead of using the session cache

## Value

a version string, e.g. `"v8"`
