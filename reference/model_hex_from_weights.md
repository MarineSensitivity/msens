# Interpolate one model's source values onto hexes with precomputed weights

Applies
[`hex_grid_weights()`](http://marinesensitivity.org/msens/reference/hex_grid_weights.md)
output to a model's (sparse) source values:
`hex value = sum(w * value over present source neighbours) / w_total`.
Absent source cells contribute 0 to the numerator but are still counted
in `w_total`, so the interpolated surface decays smoothly to 0 beyond
the model's footprint.

## Usage

``` r
model_hex_from_weights(
  con,
  model_vals,
  mdl_seq,
  threshold = 0,
  w_tbl = "hex_src_w"
)
```

## Arguments

- con:

  DuckDB connection

- model_vals:

  data.frame(`src_i`, `value`) — the model's non-absent source cells
  only (e.g. a species' occupied HCAF cells with suitability 1-100)

- mdl_seq:

  model id stamped on the output

- threshold:

  drop interpolated hex values below this (default 0)

- w_tbl:

  weight-table base name from
  [`hex_grid_weights()`](http://marinesensitivity.org/msens/reference/hex_grid_weights.md)

## Value

a tibble(mdl_seq, hex_id, value)
