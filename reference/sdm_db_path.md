# Path to SDM DuckDB

Get the file path to the species distribution model DuckDB database. v3
lives under `<data>/derived/sdm_v3.duckdb`; v4+ lives under
`<big>/<version>/sdm.duckdb`.

## Usage

``` r
sdm_db_path(version = "v6")
```

## Arguments

- version:

  version suffix (default: "v6")

## Value

character path to the DuckDB file

## Details

**Falls back to `serve.duckdb`** when the full `sdm.duckdb` is absent.
The server deliberately does not carry the multi-GB v8 database — it
holds only the KB-sized view DB over the released Parquet — so without
this fallback anything calling
[`sdm_db_con()`](http://marinesensitivity.org/msens/reference/sdm_db_con.md)
there (notably the `/report` endpoint) fails outright on v8. The Shiny
apps already did this inline; centralising it here stops the two from
drifting.
