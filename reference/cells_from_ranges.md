# Rasterize range polygons onto the global 0.05° cell grid (whole range)

Turns presence polygons (an IUCN/BOTW/NMFS range, a critical-habitat
polygon) into `(cell_id, value)` on the v8 grid, capturing the **whole**
distribution — land AND ocean (v8 does not mask land). Uses
`exactextractr`, which is fast and robust to messy geometry
(self-intersections, MULTISURFACE — the usual range-map "funk"). By
default every cell the polygon overlaps gets `value` (100 = presence);
`cover = TRUE` weights by the fraction of each cell covered. Marine
share is separate — see
[`cells_pct_marine()`](http://marinesensitivity.org/msens/reference/cells_pct_marine.md).

## Usage

``` r
cells_from_ranges(
  x,
  cellid_tif,
  value = 100,
  cover = FALSE,
  min_coverage = 0,
  max_vertices = 1000,
  exact = FALSE
)
```

## Arguments

- x:

  an `sf`/`SpatVector` of (multi)polygons in EPSG:4326 -180,180

- cellid_tif:

  path to the GLOBAL cell-id COG (`cell_id` for every cell)

- value:

  value for a covered cell. For range datasets this is the species'
  **extinction-risk score from
  [`compute_er_score()`](http://marinesensitivity.org/msens/reference/compute_er_score.md)**
  (e.g. `compute_er_score("NMFS:EN")` = 100, `"NMFS:TN"` = 50,
  `"IUCN:CR"` = 50) — NEVER a hard-coded number. Default 100 is only a
  convenience.

- cover:

  logical; `TRUE` = coverage fraction × `value`, `FALSE` (default) =
  flat `value` for any overlap

- min_coverage:

  keep cells whose covered fraction exceeds this (default 0 = any
  overlap; use 0.5 for a majority/centre-like rule)

- max_vertices:

  (exact path only) subdivide the range into chunks of at most this many
  vertices before rasterizing (default 1000). A globe-spanning range (a
  wide-ranging seabird) is one massive multipolygon that hangs
  `st_union` / `exact_extract`; subdividing bounds each chunk's raster
  window so it never touches the whole-globe polygon. Cells are
  de-duplicated afterward, so chunking is equivalent to a union here.
  Needs the `lwgeom` package; without it, falls back to the single-union
  path.

- exact:

  force the coverage-aware `exact_extract` path even for the flat
  any-overlap case (default `FALSE`). By default the flat case
  (`cover=FALSE`, `min_coverage=0`) uses the ~2x-faster
  [`terra::rasterize`](https://rspatial.github.io/terra/reference/rasterize.html)
  touches path, which yields an identical cell set; set `TRUE` for the
  boundary-exact coverage\>0 behavior. `cover=TRUE` or `min_coverage>0`
  always use the exact path.

## Value

a tibble `(cell_id integer, val double)`; empty if no overlap

## Examples

``` r
if (FALSE) { # \dontrun{
r <- msens::cells_from_ranges(ply_sp, cellid_tif)               # presence = 100, whole range
r <- msens::cells_from_ranges(ply_sp, cellid_tif, cover = TRUE) # coverage-weighted
} # }
```
