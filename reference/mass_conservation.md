# Mass-conservation ratio between a source model and its hex interpolation (pure)

Compares an integral of the original model against the same integral
computed over the interpolated hexes (e.g. Σ density·area). Returns the
ratio and whether it is within `tol` of 1.

## Usage

``` r
mass_conservation(total_source, total_hex, tol = 0.1)
```

## Arguments

- total_source:

  scalar integral over the source representation

- total_hex:

  scalar integral over the interpolated hexes

- tol:

  allowed fractional deviation from 1 (default `0.1`)

## Value

a named list: `ratio`, `within` (logical), `tol`
