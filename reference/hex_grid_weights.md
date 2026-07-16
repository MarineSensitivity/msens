# Precompute IDW weights from a shared source grid to hex centroids

One kNN (hex centroids -\> source points, great-circle via a unit-sphere
kd-tree) whose weights are reused by every model on that source grid.
Writes `<out_tbl>` (hex_id, src_i, w) with `k` rows per hex, and
`<out_tbl>_sum` (hex_id, w_total) = the IDW denominator (sum of w over
the k neighbours, constant per hex regardless of model).

## Usage

``` r
hex_grid_weights(
  con,
  hex_tbl,
  src,
  k = 8L,
  power = 2,
  where = NULL,
  radius_km = NULL,
  out_tbl = "hex_src_w",
  chunk = 5000000L
)
```

## Arguments

- con:

  DuckDB connection (opened `bigint="integer64"`)

- hex_tbl:

  hex table with `hex_id` BIGINT

- src:

  data.frame with `src_i` (integer) + `lon`,`lat` source-grid points

- k, power:

  IDW parameters (default k=8, power=2)

- where:

  optional SQL predicate on `hex_tbl` (e.g. `"in_usa"`) to limit the
  hexes weighted (a US-only score layer needs only US hexes)

- radius_km:

  optional great-circle cap (km): drop source neighbours beyond this
  distance so distant points cannot extrapolate. Paired with a full
  source grid (ocean + absent cells), this reproduces the zero-surround
  IDW fade — a hex beyond ~1 cell of any present value has only
  absent (0) neighbours in range and decays to 0. `NULL` = keep all k
  neighbours (default).

- out_tbl:

  base name for the weight tables (default "hex_src_w")

- chunk:

  hex rows per kd-tree batch (default 5e6)

## Value

invisibly, `out_tbl`
