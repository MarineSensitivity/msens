# Species table aggregated across a set of cells

Returns a tibble with one row per species, aggregated across the
supplied cell set with `pct_covered` weighting. Works against **both**
the v7 and v8 schemas — the differing column names
(`is_ok`/`is_valid_usa`, `mdl_seq`/`ms_merge_key`, `value`/`val`) are
resolved per connection.

## Usage

``` r
species_for_cells(con, cells)
```

## Arguments

- con:

  a DBI connection to an `sdm.duckdb`

- cells:

  a tibble with `cell_id` and `pct_covered`, e.g. from
  [`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md)

## Value

tibble, one row per species; `mdl_key` is the model id as character (the
v7 `mdl_seq` or the v8 `mdl_key`)

## Details

Use
[`species_for_zone()`](http://marinesensitivity.org/msens/reference/species_for_zone.md)
instead for a whole zone (subregion / Program Area / ecoregion): it
resolves the zone's cells inside the database rather than shipping
hundreds of thousands of cell ids into the query.
