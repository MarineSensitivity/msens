# Longitude/latitude of cell centers on a grid

Row-major, 1-based, top-left origin — the same arithmetic
[`publish_cog()`](http://marinesensitivity.org/msens/reference/publish_cog.md)
uses. On a `lon360` grid the returned longitude is wrapped to
`[-180, 180)`.

## Usage

``` r
cell_lonlat(cell_id, grid)
```

## Arguments

- cell_id:

  integer vector of cell ids

- grid:

  grid spec from
  [`grid_spec_for()`](http://marinesensitivity.org/msens/reference/grid_spec_for.md)

## Value

a data frame with `lon`, `lat`
