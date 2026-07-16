# Interpolate source values onto hex centroids (inverse-distance weighting)

The canonical hex interpolation: for every hexagon in `hex_tbl`, take
the `k` nearest source points (by great-circle distance, via a
unit-sphere kd-tree so it is correct at the poles and antimeridian) and
compute the inverse-distance weighted mean (`weight = 1 / d^power`) of
each `val_cols` column, skipping NA source values per column. The
interpolated columns are appended to `hex_tbl`. For a regular-grid
source this reproduces a bilinear-style interpolation; for scattered
sources it is classic IDW. This is the ONLY sanctioned way to move
values onto hexes — never inherit a containing pixel's value.

## Usage

``` r
hex_interp_idw(
  con,
  src,
  hex_tbl,
  val_cols,
  k = 8L,
  power = 2,
  lon = "lon",
  lat = "lat",
  chunk = 5000000L
)
```

## Arguments

- con:

  a DuckDB connection (opened `bigint = "integer64"`)

- src:

  a data.frame of source points with `lon`, `lat`, and `val_cols`

- hex_tbl:

  name of the DuckDB hex table (must have `hex_id` BIGINT)

- val_cols:

  character vector of value columns in `src` to interpolate

- k:

  number of nearest source points (default 8)

- power:

  IDW power (default 2)

- lon, lat:

  coordinate column names in `src` (default "lon","lat")

- chunk:

  hex rows processed per kd-tree query batch (default 5e6)

## Value

invisibly, `hex_tbl`
