# Build an msens cell tile URL template

Returns an XYZ tile URL template (with `{z}/{x}/{y}` placeholders) for
the msens TiTiler factory. The SQL is canonicalized (whitespace
collapsed) then base64url-encoded. Consistent canonicalization is
critical so repeated calls with equivalent SQL produce identical URLs —
Varnish keys on the full URL.

## Usage

``` r
cell_tile_url(
  sql = NULL,
  colormap = "spectral_r",
  rescale = NULL,
  color = NULL,
  mtime = NULL,
  mdl_key = NULL,
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

- color:

  character(1); hex `#rrggbb` or `#rrggbbaa` for a single-color mask.
  When set, the server renders every valid pixel in this flat color
  (ignoring `colormap` + `rescale`) — useful for binary "cell is present
  / cell is outside X" overlays.

- mtime:

  character; optional cache-bust tag, typically the mtime of the source
  DuckDB file (e.g. from `file.info(sdm_db)$mtime`). Distinct from the
  data version tag (`v6`, `v7`, ...) used in paths.

- mdl_key:

  character(1); the STABLE model key fast-path. When given (and `sql`
  `NULL`), the tile reads exactly one serving partition by exact path —
  the merged-model equivalent of a dense SQL point query, with no S3
  LIST. titiler resolves `mdl_key` -\> the internal integer partition id
  from the `model` registry, so this URL keeps referencing the same
  model across releases even as that internal id is renumbered.

- base:

  character; base URL of the titilecache service

## Value

character(1) tile URL template
