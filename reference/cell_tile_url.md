# Build an msens cell tile URL template

Returns an XYZ tile URL template (with `{z}/{x}/{y}` placeholders) for
the msens TiTiler factory. The SQL is canonicalized (whitespace
collapsed) then base64url-encoded. Consistent canonicalization is
critical so repeated calls with equivalent SQL produce identical URLs —
Varnish keys on the full URL.

## Usage

``` r
cell_tile_url(
  sql,
  colormap = "spectral_r",
  rescale = NULL,
  v = NULL,
  base = "https://titilecache.marinesensitivity.org"
)
```

## Arguments

- sql:

  character(1); SELECT returning `cell_id` and `value` columns

- colormap:

  character; rio-tiler colormap name (default: "spectral_r")

- rescale:

  numeric length-2 `c(min, max)` for normalization; `NULL` lets the
  server auto-compute from the SQL result on each tile request (prefer a
  client-side value from
  [`cell_stats()`](http://marinesensitivity.org/msens/reference/cell_stats.md)
  for cache stability)

- v:

  character; optional cache-bust tag (e.g. DB build date)

- base:

  character; base URL of the titilecache service

## Value

character(1) tile URL template
