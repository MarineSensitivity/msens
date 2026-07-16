# Turtle multiplicative merge rule

Sea turtles merge differently: the DPS extinction-risk surface
(`turtle_ds`) is multiplied by the AquaMaps suitability (`suit_ds`),
floored at 1 over the ER footprint, then critical-habitat datasets
(`ch_keys`) override with a max.
`val = greatest(1, round(er * suit / 100))` then `greatest(that, ch)`.
Reads a source relation `src` with
`(ms_merge_key, ds_key, cell_id, val)`.

## Usage

``` r
turtle_sql(turtle_ds, suit_ds, ch_keys, src = "turtle_src")
```

## Arguments

- turtle_ds:

  character; ds_key of the turtle DPS extinction-risk dataset.

- suit_ds:

  character; ds_key of the AquaMaps suitability dataset (usually
  `"am"`).

- ch_keys:

  character vector of critical-habitat ds_keys that override with a max
  (may be empty).

- src:

  character; name of the source relation to read (default
  `"turtle_src"`).

## Value

SQL string selecting `(mdl_key, cell_id, val)` — the whole-range turtle
surface.
