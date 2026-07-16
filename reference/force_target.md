# Should this target be forced to rebuild?

Reads two env vars: `MSENS_FORCE_ALL` (global truthy → force everything)
and `MSENS_FORCE` (comma-separated target names). Notebooks pass the
result to
[`write_manifest()`](http://marinesensitivity.org/msens/reference/write_manifest.md)
`force=` and/or use it to gate their own expensive rebuild.

## Usage

``` r
force_target(target)
```

## Arguments

- target:

  target name

## Value

logical
