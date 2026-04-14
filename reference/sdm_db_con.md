# Connect to SDM DuckDB

Open a DBI connection to the species distribution model DuckDB database.

## Usage

``` r
sdm_db_con(version = "v6", read_only = TRUE)
```

## Arguments

- version:

  version suffix (default: "v6")

- read_only:

  logical; open in read-only mode (default: TRUE)

## Value

DBI connection object
