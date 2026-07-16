# Fetch msens cell value statistics for a SQL query

Calls the msens TiTiler factory `/statistics` endpoint. Returns a named
list with `n`, `min`, `max`, `mean`, `std`, `p2`, `p50`, `p98`. Use to
set a stable legend rescale that doesn't depend on per-tile computation.

## Usage

``` r
cell_stats(
  sql = NULL,
  mtime = NULL,
  mdl_key = NULL,
  base = "https://titiler-v8.marinesensitivity.org"
)
```

## Arguments

- sql:

  character(1); same SELECT passed to
  [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)

- mtime:

  character; optional cache-bust tag, see
  [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)

- mdl_key:

  character(1); stable model key fast-path, see
  [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)

- base:

  character; base URL of the tile service (v8 `titiler-v8`; see
  [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)
  on why the legacy v7 `titilecache` must not be used)

## Value

named list of numeric statistics
