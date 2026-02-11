# Connect to SDM DuckDB

Open a DBI connection to the species distribution model DuckDB database.

## Usage

``` r
sdm_db_con(version = "v3", read_only = FALSE)
```

## Arguments

- version:

  version suffix (default: "v3")

- read_only:

  logical; open in read-only mode (default: FALSE)

## Value

DBI connection object
