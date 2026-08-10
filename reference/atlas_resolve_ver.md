# Resolve a requested version to a real, published one

The single entry point every caller (app URL `?ver=`, report endpoint,
notebook) should use. `NULL`/`""`/`"latest"` resolve to
[`atlas_latest()`](http://marinesensitivity.org/msens/reference/atlas_latest.md);
anything else must name a version present in
[`atlas_versions()`](http://marinesensitivity.org/msens/reference/atlas_versions.md).
A **pre-release is never returned for "latest"** — it is reachable only
by asking for it by name.

## Usage

``` r
atlas_resolve_ver(
  ver = NULL,
  base = atlas_base_url(),
  allow = c("released", "prerelease", "retired"),
  refresh = FALSE
)
```

## Arguments

- ver:

  requested version, or `NULL`/`"latest"`

- base:

  atlas base URL from
  [`atlas_base_url()`](http://marinesensitivity.org/msens/reference/atlas_base_url.md)

- allow:

  statuses that may be resolved when named explicitly

- refresh:

  re-fetch the registry instead of using the session cache

## Value

a validated version string
