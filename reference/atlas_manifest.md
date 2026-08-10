# A version's manifest — the contract between a release and the apps

`{ver}/manifest.json` declares everything needed to render that version,
so the app holds no per-version knowledge and publishing v9 needs no app
edit. Required keys are asserted here rather than defaulted: a **missing
`capabilities` block must fail loudly**, since defaulting an unknown
capability to "supported" makes the app promise a panel the data cannot
fill.

## Usage

``` r
atlas_manifest(ver = NULL, base = atlas_base_url(), refresh = FALSE)
```

## Arguments

- ver:

  version (resolved through
  [`atlas_resolve_ver()`](http://marinesensitivity.org/msens/reference/atlas_resolve_ver.md)
  if `NULL`/`"latest"`)

- base:

  atlas base URL from
  [`atlas_base_url()`](http://marinesensitivity.org/msens/reference/atlas_base_url.md)

- refresh:

  re-fetch instead of using the session cache

## Value

a list, with the resolved version in `$ver`
