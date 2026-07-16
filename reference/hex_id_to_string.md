# Hex-string form of BIGINT H3 ids (for display)

Hex-string form of BIGINT H3 ids (for display)

## Usage

``` r
hex_id_to_string(hex_id, con = NULL)
```

## Arguments

- hex_id:

  integer64/numeric vector of BIGINT H3 ids

- con:

  optional DuckDB connection (a temporary one is used if `NULL`)

## Value

character vector of H3 strings (e.g. "8729a411cffffff")
