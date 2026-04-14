# Species table aggregated across a set of cells

Returns a tibble with one row per species, aggregated across the
supplied cell set with `pct_covered` weighting. Column shape matches the
inline query in the mapgl app's drawn-polygon path.

## Usage

``` r
species_for_cells(con, cells)
```

## Arguments

- con:

  a DBI connection (e.g. from
  [`sdm_db_con()`](http://marinesensitivity.org/msens/reference/sdm_db_con.md))

- cells:

  a tibble from
  [`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md)

## Value

tibble
