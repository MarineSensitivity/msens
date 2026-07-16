# H3 cell ids for lon/lat points

Indexes points to H3 cells at `res` via the DuckDB `h3` extension. Ids
are returned as **BIGINT**
([`bit64::integer64`](https://bit64.r-lib.org/reference/bit64-package.html)
in R) — every valid H3 index reserves bit 63, so it fits in signed
64-bit, and BIGINT matches the OBIS `h3t` store (`idx_h3.cell_id`) for
cast-free joins. Use
[`hex_id_to_string()`](http://marinesensitivity.org/msens/reference/hex_id_to_string.md)
for the hex-string form.

## Usage

``` r
hex_id_from_lonlat(lon, lat, res = HEX_RES, con = NULL)
```

## Arguments

- lon, lat:

  numeric vectors of longitude / latitude (EPSG:4326)

- res:

  H3 resolution (default
  [HEX_RES](http://marinesensitivity.org/msens/reference/HEX_RES.md))

- con:

  optional DuckDB connection (a temporary integer64 one if `NULL`)

## Value

an `integer64` vector of H3 ids, aligned to the inputs
