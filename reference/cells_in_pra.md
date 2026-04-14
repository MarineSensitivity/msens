# Cells belonging to a Program Area zone

Fast lookup of the cells making up a Program Area by reading directly
from the `zone` / `zone_cell` tables, avoiding the
[`terra::rasterize()`](https://rspatial.github.io/terra/reference/rasterize.html)
cost paid by
[`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md).
Returns the same shape (`cell_id`, `pct_covered`) so downstream helpers
can consume it interchangeably; `pct_covered` is always 100 because
`zone_cell` membership is binary.

## Usage

``` r
cells_in_pra(con, pra_key)
```

## Arguments

- con:

  a DBI connection (e.g. from
  [`sdm_db_con()`](http://marinesensitivity.org/msens/reference/sdm_db_con.md))

- pra_key:

  Program Area key (e.g. "CGM")

## Value

tibble(cell_id integer, pct_covered integer = 100L)
