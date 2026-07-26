# Build the precomputed zone x taxon summary table

Computes
[`species_for_zone()`](http://marinesensitivity.org/msens/reference/species_for_zone.md)
for **every** zone in the `zone` table and writes the result as a single
`zone_taxon` table.

## Usage

``` r
build_zone_taxon(con, overwrite = TRUE)
```

## Arguments

- con:

  a DBI connection to the FULL `sdm.duckdb` (local `model_cell`)

- overwrite:

  replace an existing `zone_taxon` table

## Value

invisibly, the number of rows written

## Details

WHY PRECOMPUTE. v7 shipped a `zone_taxon` table and v8 dropped it, on
the assumption the app could aggregate live. It can locally — but **not
on the server**, which holds only the KB-sized `serve.duckdb` whose
`model_cell` is a view over S3 Parquet *partitioned by `mdl_id`* for
per-model point reads (titiler tiles). A zone-wide aggregation there
means listing and scanning the whole 580M-row dataset over HTTPS; in
practice it fails outright with
`IO Error: ... HTTP GET .../serve/model_cell/`. Precomputing here —
where `model_cell` is local — turns that into a few-MB table the app
just reads.
