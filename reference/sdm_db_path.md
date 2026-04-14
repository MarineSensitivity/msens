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
