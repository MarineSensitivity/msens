# Connect to SDM DuckDB

Open a DBI connection to the species distribution model DuckDB database.

## Usage

``` r
sdm_db_con(version = "2026", read_only = FALSE)
```

## Arguments

- version:

  version date suffix (default: "2026")

- read_only:

  logical; open in read-only mode (default: FALSE)

## Value

DBI connection object
