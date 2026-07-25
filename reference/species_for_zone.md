# Species table aggregated across a zone

One row per species within a named zone (`subregion_key`,
`programarea_key` or `ecoregion_key`), aggregated with `pct_covered`
weighting.

## Usage

``` r
species_for_zone(con, zone_fld, zone_val)
```

## Arguments

- con:

  a DBI connection to an `sdm.duckdb`

- zone_fld:

  zone field, e.g. `"programarea_key"`

- zone_val:

  zone value, e.g. `"GAA"`

## Value

tibble, same shape as
[`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md)

## Details

Computed live from `zone_cell` + `model_cell` + `taxon` rather than read
from a precomputed table: v7 shipped a `zone_taxon` table, but **v8 does
not build one**, which left the app's species table broken. Measured on
the v8 database, the largest zone (`subregion_key = "USA"`, ~349k cells,
~10k species) takes ~5 s, so precomputation is not required.
